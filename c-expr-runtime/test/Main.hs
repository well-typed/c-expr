{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow (first)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.String (IsString (fromString))
import Data.Text qualified as Text (pack)
import Data.Traversable (for)
import Data.Type.Nat
import Data.Vec.Lazy (Vec (..))
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Exit
import System.FilePath (takeDirectory, (</>))

import C.Type
import C.Type.Internal.Universe

import Clang.Args qualified as Clang

import C.Operators (BinaryOp (..), Op (..), UnaryOp (..), opResType, pprOp,
                    pprOpApp)
import CallClang (CType (..), getExpansionTypeMapping, queryClangForResultType)

--------------------------------------------------------------------------------

-- | The bundled @musl@ standard library headers we pass to Clang for the
-- testsuite, so that results do not depend on whichever C headers happen to be
-- installed on the host. They live in @c-expr-runtime/musl-include@ (an
-- @extra-source-file@, not installed for library consumers), so we locate them
-- relative to the working directory: @cabal test@ runs in the package
-- directory, but we also search ancestors so the lookup is robust.
findMuslInclude :: IO (Maybe FilePath)
findMuslInclude = getCurrentDirectory >>= go
  where
    go :: FilePath -> IO (Maybe FilePath)
    go dir = do
      let candidate = dir </> "musl-include" </> "x86_64"
      here <- doesDirectoryExist candidate
      if here
        then return (Just candidate)
        else let parent = takeDirectory dir
             in if parent == dir then return Nothing else go parent

-- | A hard-to-miss warning printed when the bundled @musl@ headers cannot be
-- found and we fall back to the host's system headers.
muslWarning :: String
muslWarning = unlines
  [ "#############################################################################"
  , "##                                                                         ##"
  , "##  WARNING: bundled musl headers ('musl-include/x86_64') not found!       ##"
  , "##                                                                         ##"
  , "##  Falling back to whatever C system headers exist on this machine.       ##"
  , "##  Results may differ from the musl-based reference and tests may fail    ##"
  , "##  spuriously. Make sure 'c-expr-runtime/musl-include' is present.        ##"
  , "##                                                                         ##"
  , "#############################################################################"
  ]

main :: IO ()
main = do
  let stdClangArg = "-std=c17"  -- C23 arg depends on libclang version
  clangArgs <- fmap (Clang.ClangArgs . (stdClangArg :)) $
    case platformOS hostPlatform of
      Windows -> return ["-target", "x86_64-unknown-mingw32"]  -- GHC target
      Posix -> do
        includeArgs <- findMuslInclude >>= \case
          Just muslDir -> return ["-I", fromString muslDir]
          Nothing      -> do
            putStr muslWarning
            return []
        return $ "-target" : "x86_64-pc-linux" : includeArgs

  let extendedInts = [ PtrDiff ]
  canonTys <-
    getExpansionTypeMapping clangArgs
      [ CType $ Arithmetic $ Integral $ IntLike extInt
      | extInt <- extendedInts
      ]

{-
  -- Quick debugging
  putStrLn $ "Canonical type mapping: " ++ show canonTys
  let intTy  = Arithmetic $ Integral $ IntLike $ Int Signed
      ptrTy1 = Ptr $ Arithmetic $ Integral $ IntLike $ Int Signed
  testRes <- queryClangForResultType ( ptrTy1 ::: intTy ::: VNil ) ( pprOpApp ( BinaryOp MRelEQ ) )
  putStrLn $ "Result of ty_1* == int: " ++ show testRes
-}

  putStrLn "Unary operators"
  unaries <- unaryTests hostPlatform clangArgs canonTys
  badUnary <-
    fmap catMaybes <$> for unaries $ \ ( op, tests ) -> do
      putStrLn $ pprOp ( UnaryOp op )
      let ( ok, bad ) = partitionTests tests
      if null bad
      then do
        putStrLn $ "   PASSED (" ++ show (length ok) ++ " tests)"
        return Nothing
      else do
        putStrLn $ unlines $
          ( "   FAILED:" )
          : map ( showFailure . first show ) bad
        return $ Just bad
  putStrLn "Binary operators"
  binaries <- binaryTests hostPlatform clangArgs canonTys
  badBinary <-
    fmap catMaybes <$> for binaries $ \ ( op, tests ) -> do
      putStrLn $ pprOp ( BinaryOp op )
      let ( ok, bad ) = partitionTests tests
      if null bad
      then do
        putStrLn $ "   PASSED (" ++ show (length ok) ++ " tests)"
        return Nothing
      else do
        putStrLn $ unlines $
          ( "   FAILED:" )
          : map ( showFailure . first show ) bad
        return $ Just bad
  if null badUnary && null badBinary
  then exitSuccess
  else exitFailure


showFailure :: ( String, ( Maybe CType, Maybe CType ) ) -> String
showFailure ( input, ( mbOurs, mbClang ) ) =
  unlines
    [ "   " ++ input
    , "     - computed type: " ++ showMaybeType mbOurs
    , "     -  Clang's type: " ++ showMaybeType mbClang
    ]
  where
    showMaybeType Nothing     = "<n/a>"
    showMaybeType ( Just ty ) = show ty

data TestResult a
  = TestOK !a
  | TestFailed
    { ours, clang's :: !a }
  deriving stock Show

partitionTests :: [ ( x, TestResult a ) ] -> ( [ ( x, a ) ], [ ( x, ( a, a ) ) ] )
partitionTests = foldMap $ \case
  ( x, TestOK a ) -> ( [ ( x, a ) ], [] )
  ( x, TestFailed b1 b2 ) -> ( [], [ ( x, ( b1, b2 ) ) ] )

eqTypeUpToExpansion :: Map CType CType -> Maybe CType -> Maybe CType -> Bool
eqTypeUpToExpansion canonTys ourTy clangTy = go ourTy
  where
    go mbTy
      | mbTy == clangTy
      = True
      | Just ty <- mbTy
      , Just ty' <- Map.lookup ty canonTys
      = go ( Just ty' )
      | otherwise
      = False

unaryTests :: Platform -> Clang.ClangArgs -> Map CType CType -> IO [ ( UnaryOp, [ ( CType, TestResult ( Maybe CType ) ) ] ) ]
unaryTests platform clangArgs canonTys =
  sequence
    [ ( op, ) <$> sequence
         [ do let ours = fmap CType $ opResType platform ( UnaryOp op ) ( ty ::: VNil )
              clang's <- queryClangForResultType clangArgs ( CType ty ::: VNil ) ( pprOpApp ( UnaryOp op ) )
              return $ ( CType ty , ) $
                if eqTypeUpToExpansion canonTys ours clang's
                then TestOK ours
                else TestFailed { ours, clang's }
         | ( ty ::: VNil ) <- mkCTypes <$> enumerateTypeTuples @( S Z )
         ]
    | op <- [ ( minBound :: UnaryOp ) .. maxBound ] ]


binaryTests :: Platform -> Clang.ClangArgs -> Map CType CType -> IO [ ( BinaryOp, [ ( ( CType, CType ), TestResult ( Maybe CType ) ) ] ) ]
binaryTests platform clangArgs canonTys =
  sequence
    [ ( op, ) <$>
      sequence
        [ do let ours = fmap CType $ opResType platform ( BinaryOp op ) ( ty1 ::: ty2 ::: VNil )
             clang's <- queryClangForResultType clangArgs ( CType ty1 ::: CType ty2 ::: VNil ) ( pprOpApp ( BinaryOp op ) )
             return $ ( ( CType ty1, CType ty2 ), ) $
               if eqTypeUpToExpansion canonTys ours clang's
               then TestOK ours
               else TestFailed { ours, clang's }
        | ( ty1 ::: ty2 ::: VNil ) <- mkCTypes <$> enumerateTypeTuples @( S ( S Z ) )
        ]
    | op <- [ ( minBound :: BinaryOp ) .. maxBound ] ]

mkCTypes :: Vec n ( Type OpaqueTy ) -> Vec n ( Type CType )
mkCTypes = fmap $ fmap $ \ ( OpaqueTy i ) -> TypeDef $ Text.pack ( "ty_" ++ show i )
