module C.Expr.Parse.Expr (parseMacroBody, parseMacroType) where

import Control.Monad
import Data.Foldable qualified as Foldable
import Data.Functor.Identity
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Type.Nat
import Data.Vec.Lazy (Vec (..))
import Data.Vec.Lazy qualified as Vec
import DeBruijn (Idx (..))
import Text.Parsec hiding (parseTest, token)
import Text.Parsec.Expr

import C.Expr.Parse.Identifier
import C.Expr.Parse.Infra
import C.Expr.Parse.Literal
import C.Expr.Syntax

import Clang.CStandard
import Clang.Enum.Simple
import Clang.HighLevel.Types
import Clang.LowLevel.Core

{-------------------------------------------------------------------------------
  Top-level

  Some useful references:

  - Section 6.10 "Preprocessing directives" of the C standard
    <https://fog.misty.com/perry/osp/standard/preproc.pdf>
  - Section 3 "Macros" of the @cpp@ documentation
    <https://gcc.gnu.org/onlinedocs/cpp/Macros.html>
  - "C operator precedence"
    <https://en.cppreference.com/w/c/language/operator_precedence>
-------------------------------------------------------------------------------}

-- | Parse a macro body (type or value expression)
--
-- The formal parameters are given in source order, and references to them in
-- the body become 'LocalParam's.
--
-- The entire token stream must be the body: 'parseMacroBody' ends with 'eof'.
--
-- Tries to parse the body as a type expression first. A valid type token
-- sequence always produces @'Term' ('Type' …)@; everything else parses as an
-- expression. Only when typechecking macros, we can fully discriminate type and
-- value expressions.
parseMacroBody ::
     forall ctx.
     ClangCStandard
  -> Vec ctx Identifier  -- ^ Formal parameters, in source order
  -> Parser (Expr ctx (Ps ()))
parseMacroBody cStd params = do
    rejectPragma
    -- The 'eof' inside the 'try' is essential: if 'macroType' succeeds on a
    -- prefix (e.g. the bare identifier in @size_t + 1@) but leaves tokens
    -- unconsumed, the whole attempt is abandoned and we fall back to the
    -- expression parser.
    try (macroType cStd scope <* eof) <|> (exprTuple cStd scope <* eof)
  where
    scope :: Scope ctx
    scope = mkScope params

-- | Reject macro bodies that begin with the @_Pragma@ operator
--
-- @_Pragma@ is the C99 preprocessor operator (C standard §6.10.9), equivalent
-- to @#pragma@. A macro whose body uses it, such as
--
-- > #define PACK_START _Pragma("pack(1)")
--
-- expands to a preprocessing directive, not a C value or type expression. We
-- reject such macros here rather than misparsing @_Pragma@ as a reference to a
-- variable of that name, which would otherwise survive parsing and only fail
-- later (during name resolution or typechecking) with a misleading message.
rejectPragma :: Parser ()
rejectPragma =
    notFollowedBy (token isPragma) <?> "C expression (not a _Pragma operator)"
  where
    isPragma :: Token SourcePath TokenSpelling -> Maybe ()
    isPragma t
      | getTokenSpelling (tokenSpelling t) == "_Pragma" = Just ()
      | otherwise                                       = Nothing

{-------------------------------------------------------------------------------
  Macro parameters in scope
-------------------------------------------------------------------------------}

-- | The formal parameters of a macro, ordered for de Bruijn lookup
--
-- The innermost binder comes first, so @FOO(x, y)@ binds @y@ to 'IZ' and @x@ to
-- @'IS' 'IZ'@. This is the reverse of the source order used wherever the
-- parameters are visible from the outside ('parseMacroBody', 'macroParams'),
-- which is why this type exists: the two orders are otherwise indistinguishable.
newtype Scope ctx = Scope (Vec ctx Identifier)

-- | Build a 'Scope' from the formal parameters in source order
mkScope :: Vec ctx Identifier -> Scope ctx
mkScope params = Scope (Vec.reverse params)

lookupParam :: forall ctx. Identifier -> Scope ctx -> Maybe (Idx ctx)
lookupParam n (Scope params) = go params
  where
    go :: forall ctx'. Vec ctx' Identifier -> Maybe (Idx ctx')
    go VNil = Nothing
    go (m ::: vec)
      | n == m    = Just IZ
      | otherwise = IS <$> go vec

-- | Is this spelling a formal parameter?
isParam :: Identifier -> Scope ctx -> Bool
isParam n = isJust . lookupParam n

-- | Match a reference to a formal parameter
--
-- Only the spelling decides, not the token kind. The preprocessor works on
-- pp-tokens, which have no keywords, so @#define F(bool) bool@ is valid C in
-- every standard, and inside the replacement list a parameter shadows every
-- other meaning of its spelling.
--
-- What @libclang@ reports is instead a property of the translation unit's
-- language options: the same @bool@ is 'CXToken_Identifier' under @-std=c17@
-- and 'CXToken_Keyword' under @-std=c2x@. Letting that classification reach
-- the grammar would make the parse depend on the C standard in force.
--
-- The shadowing is total: a spelling in scope has no meaning other than the
-- parameter. Wherever a token is matched by kind rather than offered to this
-- parser first -- 'keyword', and the tag name in 'taggedTypeLit' -- that parser
-- must consult the scope itself and fail on a parameter spelling.
paramRef :: Scope ctx -> Parser (Idx ctx)
paramRef scope = token $ \t -> do
    guard $ fromSimpleEnum (tokenKind t) `elem`
      [Right CXToken_Identifier, Right CXToken_Keyword]
    lookupParam (Identifier (getTokenSpelling (tokenSpelling t))) scope

{-------------------------------------------------------------------------------
  Types

-------------------------------------------------------------------------------}

-- | Parse a macro body as a C type expression
--
-- Recognizes the following grammar (informally):
--
-- @
--   type           ::= const? type_base const? pointer_layers?
--
--   type_base      ::= sign_specifier? int_size_keyword? 'int'?
--                    | sign_specifier? 'char'
--                    | 'float' | 'double'
--                    | 'void'
--                    | '_Bool' | 'bool'
--                    | ('struct' | 'union' | 'enum') identifier
--                    | identifier
--
--   pointer_layers ::= ('*' const?)+
-- @
--
-- Returns an @'Expr' ctx ('Ps' ())@ where:
--
-- * A spelling that is a formal parameter becomes @'Term' ('LocalParam' …)@,
--   whatever @libclang@ classifies it as; see 'paramRef'.
-- * A keyword base type becomes @'Term' ('Literal' ('TypeLit' …))@.
-- * A tagged base type (e.g. @struct Foo@) becomes @'Term' ('Var' …)@ with a
--   'NameTagged' name; the typechecker resolves it.
-- * Any other bare identifier becomes @'Term' ('Var' …)@; the typechecker
--   decides whether it names a type or a value.
-- * Each @const@ qualifier wraps the expression in @'TyApp' 'Const'@.
-- * Each @*@ pointer layer wraps the expression in @'TyApp' 'Pointer'@.
parseMacroType ::
     ClangCStandard
  -> Vec ctx Identifier  -- ^ Formal parameters, in source order
  -> Parser (Expr ctx (Ps ()))
parseMacroType cStd params = macroType cStd (mkScope params)

macroType :: ClangCStandard -> Scope ctx -> Parser (Expr ctx (Ps ()))
macroType cStd scope = do
    constBefore <- option False (True <$ keyword scope "const")
    base        <- typeBase cStd scope
    constAfter  <- option False (True <$ keyword scope "const")
    ptrs        <- pointerLayers scope
    -- In C, @const@ is idempotent: @const int const@ is valid but equivalent
    -- to @const int@. We therefore wrap with at most one 'Const' layer,
    -- regardless of whether the qualifier appeared before or after the base.
    let withConst
          | constBefore || constAfter = TyApp Const (base ::: VNil)
          | otherwise                 = base
    return (Foldable.foldl' apPtr withConst ptrs)
  where
    apPtr acc ptrConst =
      let withPtr = TyApp Pointer (acc ::: VNil)
      in if ptrConst then TyApp Const (withPtr ::: VNil) else withPtr

-- | Base of a type expression (without const/pointer layers)
--
-- Returns:
--
-- * @'Term' ('LocalParam' …)@ for a spelling that is a local macro parameter.
-- * @'Term' ('Literal' ('TypeLit' …))@ for keyword base types.
-- * @'Term' ('Var' …)@ with a 'NameTagged' name for a tagged base type (e.g. @struct Foo@).
-- * @'Term' ('Var' …)@ for any other bare identifier; the typechecker decides
--   whether it names a type or a value.
typeBase :: forall ctx. ClangCStandard -> Scope ctx -> Parser (Expr ctx (Ps ()))
typeBase cStd scope =
    choice [
        -- Macro parameter. Comes first: within the replacement list a parameter
        -- shadows the keyword of the same spelling, so @#define F(bool) bool@
        -- is the parameter and not the type.
        Term . LocalParam <$> paramRef scope
        -- Type literal.
      , Term . Literal . TypeLit <$> typeLiteral cStd scope
        -- Tagged type (e.g., @struct Foo@)
      , Term . mkTagged <$> taggedTypeLit scope
        -- The bare identifier (typedef name, type macro, or expression
        -- variable) is needed to parse pointer-qualified typedef references
        -- such as @size_t *@: without it, @parseMacroType@ would reject the
        -- identifier base and the expression parser would then fail on @*@ (a
        -- binary operator without a right-hand side). Attempting to detangle
        -- the two by restricting @parseMacroType@ to keyword\/tagged bases only
        -- does not help (both paths produce the same @'Var'@ node) while
        -- adding backtracking overhead.
      , Term . mkVar <$> parseIdentifier
      ]
  where
    mkTagged :: (TagKind, Identifier) -> Term ctx (Ps ())
    mkTagged (tag, ident) = Var (XVarPs ()) (NameTagged ident tag) []

    mkVar :: Identifier -> Term ctx (Ps ())
    mkVar n = Var (XVarPs ()) (NameOrdinary n) []

-- | Parse a sequence of type-literal keywords and combine them
--
-- C type literal keywords can appear in various orders:
--
-- @
--   unsigned long int
--   long unsigned int
--   int long unsigned
-- @
--
-- are all the same type.
typeLiteral :: ClangCStandard -> Scope ctx -> Parser TypeLit
typeLiteral cStd scope = do
    kws <- many1 (typeKeyword cStd scope)
    case interpretKeywords kws of
      Just lit -> return lit
      Nothing  -> fail "unrecognised type literal"

-- | Parse an elaborated type literal
--
-- @
--   struct tag
--   union  tag
--   enum   tag
-- @
--
-- A formal parameter as the tag name is rejected: 'NameTagged' has no room for
-- a de Bruijn index, so @#define F(Foo) struct Foo@ has no representation.
taggedTypeLit :: Scope ctx -> Parser (TagKind, Identifier)
taggedTypeLit scope = do
    tag  <- choice [
        TagStruct <$ keyword scope "struct"
      , TagUnion  <$ keyword scope "union"
      , TagEnum   <$ keyword scope "enum"
      ]
    name <- parseIdentifier
    guard $ not (isParam name scope)
    return (tag, name)

data TypeKeyword =
    KwSigned | KwUnsigned
  | KwShort | KwInt | KwLong | KwChar
  | KwFloat | KwDouble
  | KwVoid | KwBool
  deriving stock (Eq)

typeKeyword :: ClangCStandard -> Scope ctx -> Parser TypeKeyword
typeKeyword cStd scope = choice $
      [ KwSigned   <$ keyword scope "signed"
      , KwUnsigned <$ keyword scope "unsigned"
      , KwShort    <$ keyword scope "short"
      , KwInt      <$ keyword scope "int"
      , KwLong     <$ keyword scope "long"
      , KwChar     <$ keyword scope "char"
      , KwFloat    <$ keyword scope "float"
      , KwDouble   <$ keyword scope "double"
      , KwVoid     <$ keyword scope "void"
      , KwBool     <$ keyword scope "_Bool"
      ]
      ++
      -- @bool@ is a keyword in C23 and later.
      case cStd of
        ClangCStandard std _ | std >= C23 -> [bool]
        _                                 -> []
  where
    bool = KwBool <$ keyword scope "bool"

-- | Combine a list of type keywords into a type literal
--
-- Returns 'Nothing' if the combination is invalid.
--
-- Duplicate keywords (e.g. @signed signed int@) are accepted, since input
-- tokens come from libclang-validated C source and such constructs are
-- rejected by the C compiler long before we see them.
interpretKeywords :: [TypeKeyword] -> Maybe TypeLit
interpretKeywords kws
  -- void
  | kws == [KwVoid]
  = Just TypeVoid

  -- _Bool / bool
  | kws == [KwBool]
  = Just TypeBool

  -- float
  | kws == [KwFloat]
  = Just $ TypeFloat SizeFloat

  -- double
  | kws == [KwDouble]
  = Just $ TypeFloat SizeDouble

  -- char with optional sign
  | KwChar `elem` kws
  , let sign = extractSign kws
  , all (\k -> k `elem` [KwChar, KwSigned, KwUnsigned]) kws
  = Just $ TypeChar sign

  -- integral types: combinations of sign, size, and int
  | all (\k -> k `elem` [KwSigned, KwUnsigned, KwShort, KwInt, KwLong]) kws
  = Just $ TypeInt (extractSign kws) (extractIntSize kws)

  | otherwise
  = Nothing

extractSign :: [TypeKeyword] -> Maybe Sign
extractSign kws
  | KwSigned   `elem` kws = Just Signed
  | KwUnsigned `elem` kws = Just Unsigned
  | otherwise              = Nothing

extractIntSize :: [TypeKeyword] -> Maybe IntSize
extractIntSize kws
  | KwShort `elem` kws                   = Just SizeShort
  | length (filter (== KwLong) kws) >= 2 = Just SizeLongLong
  | KwLong `elem` kws                    = Just SizeLong
  | KwInt `elem` kws                     = Just SizeInt
  | otherwise                            = Nothing
  -- NB: @signed@ alone (no size keyword, no @int@) means @signed int@.
  -- We return Nothing here; the caller interprets (Just sign, Nothing) as int.

-- | Parse zero or more pointer indirections, optionally followed by @const@
pointerLayers :: Scope ctx -> Parser [Bool]
pointerLayers scope = many (pointerLayer scope)

-- | Parse a pointer indirection, optionally followed by @const@
pointerLayer :: Scope ctx -> Parser Bool
pointerLayer scope = do
    punctuation "*"
    option False (True <$ keyword scope "const")

-- | Match a keyword token with the given spelling
--
-- Fails when the spelling is a formal parameter. A keyword in qualifier or
-- specifier position is never offered to 'paramRef', so the shadowing has to
-- be enforced here.
keyword :: Scope ctx -> Text -> Parser ()
keyword scope expected
  | isParam (Identifier expected) scope = parserZero
  | otherwise                                         = token $ \case
      (Token k s _ _)
        | fromSimpleEnum k == Right CXToken_Keyword
          && getTokenSpelling s == expected ->
            Just ()
        | otherwise ->
            Nothing

{-------------------------------------------------------------------------------
  Simple expressions
-------------------------------------------------------------------------------}

term :: forall ctx. ClangCStandard -> Scope ctx -> Parser (Term ctx (Ps ()))
term cStd scope =
    buildExpressionParser ops trm <?> "simple expression"
  where
    trm :: Parser (Term ctx (Ps ()))
    trm = choice [
        Literal <$> lit
      , localParamOrVar
      ]

    -- As in 'typeBase', the parameter scope is consulted before the token is
    -- interpreted as a keyword or as a free variable.
    localParamOrVar :: Parser (Term ctx (Ps ()))
    localParamOrVar = choice [
          LocalParam <$> paramRef scope
        , do varName <- parseIdentifier
             Var (XVarPs ()) (NameOrdinary varName) <$>
               option [] (actualArgs cStd scope)
        ]

    lit :: Parser Literal
    lit = ValueLit <$> choice [
        ValueInt    <$> literalInteger
      , ValueFloat  <$> literalFloat
      , ValueChar   <$> literalChar
      , ValueString <$> literalString
      ]

    ops :: OperatorTable [Token SourcePath TokenSpelling] () Identity (Term ctx (Ps ()))
    ops = []


-- | Parse integer literal
literalInteger :: Parser IntegerLiteral
literalInteger = do
  (val, ty) <- parseTokenOfKind CXToken_Literal parseLiteralInteger
  return $
    IntegerLiteral
      { integerLiteralType  = ty
      , integerLiteralValue = val
      }

-- | Parse floating point literal
literalFloat :: Parser FloatingLiteral
literalFloat = do
  (fltVal, dblVal, ty) <- parseTokenOfKind CXToken_Literal parseLiteralFloating
  return $
    FloatingLiteral
      { floatingLiteralType = ty
      , floatingLiteralFloatValue = fltVal
      , floatingLiteralDoubleValue = dblVal
      }

-- | Parse character literal
literalChar :: Parser CharLiteral
literalChar = do
  val <- parseTokenOfKind CXToken_Literal parseLiteralChar
  return $ CharLiteral val

-- | Parse string literal
literalString :: Parser StringLiteral
literalString = do
  val <- parseTokenOfKind CXToken_Literal parseLiteralString
  return $ StringLiteral val

actualArgs :: ClangCStandard -> Scope ctx -> Parser [Expr ctx (Ps ())]
actualArgs cStd scope = parens $ expr cStd scope `sepBy` comma

{-------------------------------------------------------------------------------
  Expressions

  This is currently only a subset of the operators described in
  <https://en.cppreference.com/w/c/language/operator_precedence>, but we do
  follow the same structure.
-------------------------------------------------------------------------------}

exprTuple :: ClangCStandard -> Scope ctx -> Parser (Expr ctx (Ps ()))
exprTuple cStd scope = try tuple <|> expr cStd scope
  where
    tuple = do
      openParen <- optionMaybe $ punctuation "("
      (e1, e2, es) <- expr cStd scope `sepBy2` comma
      case openParen of
        Nothing -> return ()
        Just {} -> punctuation ")"
      return $
        Vec.reifyList es $ \es' ->
           VaApp NoXApp MTuple ( e1 ::: e2 ::: es' )

expr :: forall ctx. ClangCStandard -> Scope ctx -> Parser (Expr ctx (Ps ()))
expr cStd scope = buildExpressionParser ops trm <?> "expression"
  where

    trm :: Parser (Expr ctx (Ps ()))
    trm = choice [
          parens (expr cStd scope)
        , Term <$> term cStd scope
        ]

    -- 'OperatorTable' expects the list in descending precedence
    ops = [
        -- Precedence 1 (all left-to-right)
        []

        -- Precedence 2 (all right-to-left)
      , [ Prefix (ap1 MUnaryPlus  <$ punctuation "+")
        , Prefix (ap1 MUnaryMinus <$ punctuation "-")
        , Prefix (ap1 MLogicalNot <$ punctuation "!")
        , Prefix (ap1 MBitwiseNot <$ punctuation "~")
        ]

        -- Precedence 3 (precedence 3 .. 12 are all left-to-right)
      , [ Infix (ap2 MMult <$ punctuation "*") AssocLeft
        , Infix (ap2 MDiv  <$ punctuation "/") AssocLeft
        , Infix (ap2 MRem  <$ punctuation "%") AssocLeft
        ]

        -- Precedence 4
      , [ Infix (ap2 MAdd <$ punctuation "+") AssocLeft
        , Infix (ap2 MSub <$ punctuation "-") AssocLeft
        ]

        -- Precedence 5
      , [ Infix (ap2 MShiftLeft  <$ punctuation "<<") AssocLeft
        , Infix (ap2 MShiftRight <$ punctuation ">>") AssocLeft
        ]

        -- Precedence 6
      , [ Infix (ap2 MRelLT <$ punctuation "<")  AssocLeft
        , Infix (ap2 MRelLE <$ punctuation "<=") AssocLeft
        , Infix (ap2 MRelGT <$ punctuation ">")  AssocLeft
        , Infix (ap2 MRelGE <$ punctuation ">=") AssocLeft
        ]

        -- Precedence 7
      , [ Infix (ap2 MRelEQ <$ punctuation "==") AssocLeft
        , Infix (ap2 MRelNE <$ punctuation "!=") AssocLeft
        ]

        -- Precedence 8 .. 12
      , [ Infix (ap2 MBitwiseAnd <$ punctuation "&")  AssocLeft ]
      , [ Infix (ap2 MBitwiseXor <$ punctuation "^")  AssocLeft ]
      , [ Infix (ap2 MBitwiseOr  <$ punctuation "|")  AssocLeft ]
      , [ Infix (ap2 MLogicalAnd <$ punctuation "&&") AssocLeft ]
      , [ Infix (ap2 MLogicalOr  <$ punctuation "||") AssocLeft ]
      ]

    ap1 :: VaFun (S Z) -> Expr ctx (Ps ()) -> Expr ctx (Ps ())
    ap1 op arg = VaApp NoXApp op ( arg ::: VNil )

    ap2 :: VaFun (S (S Z)) -> Expr ctx (Ps ()) -> Expr ctx (Ps ()) -> Expr ctx (Ps ())
    ap2 op arg1 arg2 = VaApp NoXApp op ( arg1 ::: arg2 ::: VNil )

sepBy2 :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (a, a, [a])
{-# INLINEABLE sepBy2 #-}
sepBy2 p sep = do
  x1 <- p
  void sep
  x2 <- p
  xs <- many $ sep >> p
  return (x1, x2, xs)
