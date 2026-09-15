-- | Golden integration tests for 'C.Expr.Parse.parseMacroBody'
--
-- These tests use @libclang@ to tokenise the macros defined in
-- @test/fixtures/macros.h@, split each definition into its formal parameters
-- and its body, feed the bodies to 'parseMacroBody', and compare the results
-- against the golden file @test/fixtures/macros.golden@.
--
-- Golden file can be regenerated using the @--accept@ CLI option.
module Test.CExpr.Parse.Golden (tests) where

import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vec.Lazy qualified as Vec
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import C.Expr.Parse
import C.Expr.Syntax

import Clang.Args
import Clang.CStandard
import Clang.Enum.Bitfield
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths (getRealPathText)
import Clang.Version

import Paths_c_expr_dsl (getDataDir)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data TestCStandard = CExprC17 | CExprC23

testCStandardToCStandard :: TestCStandard -> CStandard
testCStandardToCStandard = \case
    CExprC17 -> C17
    CExprC23 -> C23

testCStandardToClangArg :: TestCStandard -> String
testCStandardToClangArg = \case
    CExprC17 -> "-std=c17"
    CExprC23 -> "-std=c2x"

tests :: TestTree
tests = testGroup "Parse.Golden" $ [goldenWith CExprC17] ++ mbC23
  where
    mbC23 =
      case runtimeClangVersion of
        ClangVersion x | x >= (15,0,0) -> [goldenWith CExprC23]
        _                              -> []

goldenWith :: TestCStandard -> TestTree
goldenWith testCStd =
    goldenDynamic ("macros-" <> show cStd)
      (datadir </> "macros." <> show cStd <> ".golden")
      (parseMacrosFixture testCStd (datadir </> "macros.h"))
  where
    cStd = testCStandardToCStandard testCStd

{-# NOINLINE datadir #-}
datadir :: FilePath
datadir = unsafePerformIO getDataDir

-- | Minimal golden test using an 'IO' action to resolve the golden file path.
--
-- If the golden file does not yet exist it is created and the test fails so
-- that the developer can inspect and commit it. If it does exist the actual
-- output is compared byte-for-byte; on a mismatch the test fails with a hint
-- about how to regenerate the file.
goldenDynamic ::
     TestName
  -> FilePath           -- ^ path to the golden file
  -> IO LBS.ByteString  -- ^ action producing the actual output
  -> TestTree
goldenDynamic name goldenPath getActual = goldenVsString name goldenPath getActual

{-------------------------------------------------------------------------------
  Run the parser on all macros in the fixture file
-------------------------------------------------------------------------------}

parseMacrosFixture :: TestCStandard -> FilePath -> IO LBS.ByteString
parseMacrosFixture testCStd fixturePath = do
    macroTokens <- collectMacroTokens testCStd fixturePath
    return $ LBS.pack $ unlines $ map formatEntry macroTokens
  where
    cStd :: ClangCStandard
    cStd = ClangCStandard (testCStandardToCStandard testCStd) DisableGnu

    formatEntry :: (Text, [Token SourcePath TokenSpelling]) -> String
    formatEntry (name, tokens) =
        Text.unpack name ++ ": " ++
          case splitMacro tokens of
            Nothing              -> parseError
            Just (params, body)  ->
              Vec.reifyList params $ \params' ->
                case runParser (parseMacroBody cStd params') body of
                  Right expr -> "Right " ++ show expr
                  Left _     -> parseError

    parseError :: String
    parseError = "Left <parse error>"

{-------------------------------------------------------------------------------
  Splitting macro definitions

  Test-local and deliberately minimal: @hs-bindgen@ owns the real splitter
  (@HsBindgen.Macro.Syntax.splitMacro@), which reports errors and handles
  variadic macros. This one exists only so that these golden tests can keep
  driving real @#define@s through libclang. Do not promote it into the library.
-------------------------------------------------------------------------------}

-- | Split a macro definition into its formal parameters and its body
--
-- The parameters are returned in source order. 'Nothing' means the definition
-- is not one we can represent: a name that is neither an identifier nor a
-- keyword, a malformed parameter list, or a variadic macro.
splitMacro ::
     [Token SourcePath TokenSpelling]
  -> Maybe ([Identifier], [Token SourcePath TokenSpelling])
splitMacro []            = Nothing
splitMacro (name:tokens)
    | not (isMacroName name) = Nothing
    | otherwise              =
        case tokens of
          -- A macro is function-like only when the opening parenthesis follows
          -- the name with no whitespace in between. See issue #1903:
          -- <https://github.com/well-typed/hs-bindgen/issues/1903>
          t:ts | adjacent name t, isPunctuation "(" t -> paramList ts
          _otherwise                                  -> Just ([], tokens)
  where
    paramList ::
         [Token SourcePath TokenSpelling]
      -> Maybe ([Identifier], [Token SourcePath TokenSpelling])
    paramList (t:ts) | isPunctuation ")" t = Just ([], ts)
    paramList ts                           = go [] ts

    go ::
         [Identifier]
      -> [Token SourcePath TokenSpelling]
      -> Maybe ([Identifier], [Token SourcePath TokenSpelling])
    go acc (t:u:us)
      | Just param <- macroParam t
      = if | isPunctuation "," u -> go (param:acc) us
           | isPunctuation ")" u -> Just (reverse (param:acc), us)
           | otherwise           -> Nothing
    go _ _
      = Nothing

-- | Macro names may be keywords (@#define bool int@ is valid C), parameter
-- names may not
isMacroName :: Token SourcePath TokenSpelling -> Bool
isMacroName t = case fromSimpleEnum (tokenKind t) of
    Right CXToken_Identifier -> True
    Right CXToken_Keyword    -> True
    _otherwise               -> False

macroParam :: Token SourcePath TokenSpelling -> Maybe Identifier
macroParam t = case fromSimpleEnum (tokenKind t) of
    Right CXToken_Identifier -> Just $ Identifier (getTokenSpelling (tokenSpelling t))
    _otherwise               -> Nothing

isPunctuation :: String -> Token SourcePath TokenSpelling -> Bool
isPunctuation expected t =
       fromSimpleEnum (tokenKind t) == Right CXToken_Punctuation
    && removeMultilines (Text.unpack (getTokenSpelling (tokenSpelling t))) == expected

-- | Are the two tokens adjacent in the source, with no whitespace in between?
adjacent ::
     Token SourcePath TokenSpelling
  -> Token SourcePath TokenSpelling
  -> Bool
adjacent prev next =
       singleLocPath   end == singleLocPath   start
    && singleLocLine   end == singleLocLine   start
    && singleLocColumn end == singleLocColumn start
  where
    end   = rangeEnd   $ multiLocExpansion <$> tokenExtent prev
    start = rangeStart $ multiLocExpansion <$> tokenExtent next

-- | Drop line continuations, which libclang sometimes leaves inside a token
-- spelling
removeMultilines :: String -> String
removeMultilines = \case
    '\\':'\n':cs -> removeMultilines cs
    c:cs         -> c : removeMultilines cs
    []           -> []

{-------------------------------------------------------------------------------
  Collect macro definitions from a C header file via libclang
-------------------------------------------------------------------------------}

collectMacroTokens ::
     TestCStandard
  -> FilePath
  -> IO [(Text, [Token SourcePath TokenSpelling])]
collectMacroTokens testCStd path =
    HighLevel.withIndex DontDisplayDiagnostics $ \index ->
      HighLevel.withTranslationUnit index src noArgs [] flags $ \unit -> do
        root    <- clang_getTranslationUnitCursor unit
        HighLevel.clang_visitChildren root (macroFold unit)
  where
    src :: Maybe SourcePath
    src = Just $ SourcePath $ Text.pack path

    noArgs :: ClangArgs
    noArgs = ClangArgs [testCStandardToClangArg testCStd]

    flags :: BitfieldEnum CXTranslationUnit_Flags
    flags = bitfieldEnum [CXTranslationUnit_DetailedPreprocessingRecord]

macroFold ::
     CXTranslationUnit
  -> Fold IO (Text, [Token SourcePath TokenSpelling])
macroFold unit = simpleFold $ \cursor -> do
    loc    <- clang_getCursorLocation cursor
    inMain <- clang_Location_isFromMainFile loc
    if not inMain
      then foldContinue
      else do
        kind <- fromSimpleEnum <$> clang_getCursorKind cursor
        case kind of
          Right CXCursor_MacroDefinition -> do
              name   <- clang_getCursorSpelling cursor
              range  <- HighLevel.clang_getCursorExtent cursor
              tokens <- HighLevel.clang_tokenize unit getRealPathText (multiLocExpansion <$> range)
              foldContinueWith (name, tokens)
          _ ->
              foldContinue
