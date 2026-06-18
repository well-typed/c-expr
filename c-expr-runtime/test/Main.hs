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
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import Data.Type.Nat
import Data.Vec.Lazy (Vec (..))
import System.Exit
import System.Info qualified as Info

import C.Type
import C.Type.Internal.Universe

import Clang.Args qualified as Clang
import Clang.Discover qualified as Clang

import C.Operators (BinaryOp (..), Op (..), UnaryOp (..), opResType, pprOp,
                    pprOpApp)
import CallClang (CType (..), getExpansionTypeMapping, queryClangForResultType)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  resourceDirArgs <- clangResourceDirArgs
  let stdClangArg = "-std=c17"  -- C23 arg depends on libclang version
      targetArgs = case platformOS hostPlatform of
        Windows -> [ "-target", "x86_64-unknown-mingw32" ]
        Posix
          -- On macOS, test against the native target and the system SDK headers
          -- rather than cross-compiling to Linux (for which the headers are
          -- absent). Linux uses an explicit target for reproducibility.
          | Info.os == "darwin" -> []
          | otherwise           -> [ "-target", "x86_64-pc-linux" ]
      clangArgs = Clang.ClangArgs $ stdClangArg : targetArgs ++ resourceDirArgs
      extendedInts = [ PtrDiff ]
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
        pure Nothing
      else do
        putStrLn $ unlines $
          ( "   FAILED:" )
          : map ( showFailure . first show ) bad
        pure $ Just bad
  putStrLn "Binary operators"
  binaries <- binaryTests hostPlatform clangArgs canonTys
  badBinary <-
    fmap catMaybes <$> for binaries $ \ ( op, tests ) -> do
      putStrLn $ pprOp ( BinaryOp op )
      let ( ok, bad ) = partitionTests tests
      if null bad
      then do
        putStrLn $ "   PASSED (" ++ show (length ok) ++ " tests)"
        pure Nothing
      else do
        putStrLn $ unlines $
            "   FAILED:"
          : map ( showFailure . first show ) bad
        pure $ Just bad
  if null badUnary && null badBinary
  then exitSuccess
  else exitFailure


clangResourceDirArgs :: IO [ String ]
clangResourceDirArgs = do
  let noTrace _ _ = pure ()
  paths <- Clang.getPaths noTrace Clang.BuiltinIncDirClang
  case Clang.pBuiltinIncDir paths of
    Just dir -> do
      putStrLn $ "Clang builtin include directory is: " ++ dir
      pure [ "-isystem", dir ]
    _ -> do
      putStrLn $ unlines [
          "WARNING: could not determine Clang's builtin include directory, falling back to libclang's own resolution."
        , "Builtin headers (stddef.h, ...) may not be found."
        ]
      pure []

showFailure :: ( String, ( Maybe CType, Maybe CType, [ Text ] ) ) -> String
showFailure ( input, ( mbOurs, mbClang, diags ) ) =
  unlines $
       [ "   " ++ input
       , "     - computed type: " ++ showMaybeType mbOurs
       , "     -  Clang's type: " ++ showMaybeType mbClang
       ]
    ++ [ "     - Clang's diagnostics:"
       | not ( null diags ) ]
    ++ [ "         " ++ l
       | d <- diags
       , l <- lines ( Text.unpack d ) ]
  where
    showMaybeType Nothing     = "<n/a>"
    showMaybeType ( Just ty ) = show ty

data TestResult a
  = TestOK !a
  | TestFailed {
        ours       :: !a
      , clang's    :: !a
      , clangDiags :: ![ Text ]
      }
  deriving stock Show

partitionTests ::
     [ ( x, TestResult a ) ]
  -> ( [ ( x, a ) ], [ ( x, ( a, a, [ Text ] ) ) ] )
partitionTests = foldMap $ \case
  ( x, TestOK a )            -> ( [ ( x, a ) ], []                        )
  ( x, TestFailed b1 b2 ds ) -> ( []          , [ ( x, ( b1, b2, ds ) ) ] )

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
         [ do let ours = CType <$> opResType platform ( UnaryOp op ) ( ty ::: VNil )
              ( clang's, clangDiags ) <-
                queryClangForResultType
                  clangArgs
                  ( CType ty ::: VNil )
                  ( pprOpApp ( UnaryOp op ) )
              pure $ ( CType ty , ) $
                if eqTypeUpToExpansion canonTys ours clang's
                then TestOK ours
                else TestFailed { ours, clang's, clangDiags }
         | ( ty ::: VNil ) <- mkCTypes <$> enumerateTypeTuples @( S Z )
         ]
    | op <- [ ( minBound :: UnaryOp ) .. maxBound ] ]


binaryTests :: Platform -> Clang.ClangArgs -> Map CType CType -> IO [ ( BinaryOp, [ ( ( CType, CType ), TestResult ( Maybe CType ) ) ] ) ]
binaryTests platform clangArgs canonTys =
  sequence
    [ ( op, ) <$>
      sequence
        [ do let ours = CType <$> opResType platform ( BinaryOp op ) ( ty1 ::: ty2 ::: VNil )
             ( clang's, clangDiags ) <-
               queryClangForResultType
                 clangArgs
                 ( CType ty1 ::: CType ty2 ::: VNil )
                 ( pprOpApp ( BinaryOp op ) )
             pure $ ( ( CType ty1, CType ty2 ), ) $
               if eqTypeUpToExpansion canonTys ours clang's
               then TestOK ours
               else TestFailed { ours, clang's, clangDiags }
        | ( ty1 ::: ty2 ::: VNil ) <- mkCTypes <$> enumerateTypeTuples @( S ( S Z ) )
        ]
    | op <- [ ( minBound :: BinaryOp ) .. maxBound ] ]

mkCTypes :: Vec n ( Type OpaqueTy ) -> Vec n ( Type CType )
mkCTypes = fmap $ fmap $ \ ( OpaqueTy i ) -> TypeDef $ Text.pack ( "ty_" ++ show i )
