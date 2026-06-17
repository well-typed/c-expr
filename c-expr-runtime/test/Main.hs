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
import Data.Text qualified as Text (pack)
import Data.Traversable (for)
import Data.Type.Nat
import Data.Vec.Lazy (Vec (..))
import System.Exit

import C.Type
import C.Type.Internal.Universe

import Clang.Args qualified as Clang

import C.Operators (BinaryOp (..), Op (..), UnaryOp (..), opResType, pprOp,
                    pprOpApp)
import CallClang (CType (..), getExpansionTypeMapping, queryClangForResultType)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  let stdClangArg = "-std=c17"  -- C23 arg depends on libclang version
      targetArgs = case platformOS hostPlatform of
        Windows -> ["-target", "x86_64-unknown-mingw32"]
        Posix   -> ["-target", "x86_64-pc-linux"]
      clangArgs = Clang.ClangArgs $ stdClangArg : targetArgs
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
