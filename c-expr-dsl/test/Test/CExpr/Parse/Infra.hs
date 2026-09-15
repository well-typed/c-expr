-- | Test infrastructure for the c-expr-dsl parser tests
module Test.CExpr.Parse.Infra (
    -- * Token constructors
    kw
  , ident
  , punc
  , lit
    -- * Running parsers
  , checkType
  , checkBody
  , parseTestWith
    -- * Results
  , tyLit
  ) where

import Data.Nat (Nat (..))
import Data.Text (Text)
import Data.Vec.Lazy (Vec (..))
import Text.Parsec (eof)

import C.Expr.Parse
import C.Expr.Syntax

import Clang.CStandard
import Clang.Enum.Simple
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import Test.CExpr.Util

{-------------------------------------------------------------------------------
  Token constructors
-------------------------------------------------------------------------------}

-- | Construct a keyword token
kw :: Text -> Token SourcePath TokenSpelling
kw = mkToken CXToken_Keyword

-- | Construct an identifier token
ident :: Text -> Token SourcePath TokenSpelling
ident = mkToken CXToken_Identifier

-- | Construct a punctuation token
punc :: Text -> Token SourcePath TokenSpelling
punc = mkToken CXToken_Punctuation

-- | Construct a literal token
lit :: Text -> Token SourcePath TokenSpelling
lit = mkToken CXToken_Literal

mkToken :: CXTokenKind -> Text -> Token SourcePath TokenSpelling
mkToken kind spelling = Token{
      tokenKind       = simpleEnum kind
    , tokenSpelling   = TokenSpelling spelling
    , tokenExtent     = Range fakeLoc fakeLoc
    , tokenCursorKind = simpleEnum CXCursor_UnexposedDecl
    }

{-------------------------------------------------------------------------------
  Running parsers
-------------------------------------------------------------------------------}

-- | Run the type parser on a sequence of tokens
--
-- Adds 'eof' so that trailing tokens are rejected as parse failures.
checkType ::
     ClangCStandard
  -> [Token SourcePath TokenSpelling]
  -> Either MacroParseError (Expr Z (Ps ()))
checkType cStd = runParser (parseMacroType cStd VNil <* eof)

-- | Run the macro body parser on a sequence of tokens
--
-- The tokens are the macro body only, without the name or the parameter list;
-- the formal parameters are given separately, in source order. 'parseMacroBody'
-- itself calls 'eof', so no trailing tokens are allowed.
checkBody ::
     ClangCStandard
  -> Vec ctx Identifier
  -> [Token SourcePath TokenSpelling]
  -> Either MacroParseError (Expr ctx (Ps ()))
checkBody cStd params = runParser (parseMacroBody cStd params)

-- | Run a parser on a list of (kind, spelling) pairs and print the result.
--
-- Useful for interactive debugging in GHCi:
--
-- > parseTestWith (parseMacroBody C17 VNil) [(CXToken_Literal, "1")]
parseTestWith ::
     Show a
  => Parser a
  -> [(CXTokenKind, Text)]
  -> IO ()
parseTestWith p pairs = print $ runParser p (map (uncurry mkToken) pairs)

{-------------------------------------------------------------------------------
  Results
-------------------------------------------------------------------------------}

tyLit :: TypeLit -> Expr ctx (Ps ())
tyLit = Term . Literal . TypeLit
