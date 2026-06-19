module Test.CExpr.Typecheck.Infra (
    -- * Macro definitions
    MacDef
  , runTcSeq
  , classifyOne
    -- * Classification predicates
  , isTypeMacro
  , isValueMacro
    -- * Assertion helpers
  , assertTypeMacro
  , assertValueMacro
    -- * Expression helpers
  , tyLit
  , constOf
  , ptrOf
  , intLit
  , add
  , shiftLeft
  , mlocal
  , mvar
  , mtagged
  ) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Nat (Nat (..))
import Data.Set qualified as Set
import Data.Vec.Lazy (Vec (..))
import Data.Void (Void)
import DeBruijn (Idx (..))
import Test.Tasty.HUnit

import C.Type qualified as Runtime

import C.Expr.Syntax
import C.Expr.Typecheck
import C.Expr.Util.Panic

import Test.CExpr.Util

type MacDef = (Identifier, Expr Z Ps)

-- | Run 'tcMacros' on a single macro
--
-- Convenience for tests that exercise one macro in isolation; threads no
-- typedef context.
classifyOne ::
     forall ctx.
     Identifier
  -> Vec ctx Identifier
  -> Expr ctx Ps
  -> MacroTcResult Void Identifier
classifyOne name params body =
    case Map.toList (runTcMacros [Macro fakeLoc name params body]) of
      ((_, x):_) -> x
      []         -> panicPure "classifyOne: unexpected empty typecheck result"

-- | Typecheck a sequence of nullary macros in order, threading each successful
-- result into the typing environment for later macros to reference.
runTcSeq :: [MacDef] -> Map Identifier (MacroTcResult Void Identifier)
runTcSeq defs =
    runTcMacros [Macro fakeLoc nm VNil body | (nm, body) <- defs]

-- | Shared 'tcMacros' driver for the test helpers: no typedefs in scope,
-- variable injection is the identity, tagged-type injection renders as
-- @"<tag> <name>"@ to match the textual form expected by tests.
--
-- Tagged-type injection never fails here (uses 'Void' as the inject error
-- type), so 'MacroTcInjectError' results are not produced.
runTcMacros :: [Macro] -> Map Identifier (MacroTcResult Void Identifier)
runTcMacros macros =
    tcMacros Set.empty (const id) id injectTaggedName macros
  where
    injectTaggedName :: Applicative m => TagKind -> Identifier -> m Identifier
    injectTaggedName tag nm = pure $ tagToPrefix tag <> " " <> nm

    tagToPrefix :: TagKind -> Identifier
    tagToPrefix = \case
      TagStruct -> "struct"
      TagUnion  -> "union"
      TagEnum   -> "enum"

isTypeMacro :: MacroTcResult e a -> Bool
isTypeMacro (MacroTcTypeExpr _) = True
isTypeMacro _                   = False

isValueMacro :: MacroTcResult e a -> Bool
isValueMacro (MacroTcValueExpr _) = True
isValueMacro _                    = False

assertTypeMacro :: (Show e, Show a) => MacroTcResult e a -> Assertion
assertTypeMacro r =
    assertBool ("expected MacroTcTypeExpr, got: " ++ show r) (isTypeMacro r)

assertValueMacro :: (Show e, Show a) => MacroTcResult e a -> Assertion
assertValueMacro r =
    assertBool ("expected MacroTcValueExpr, got: " ++ show r) (isValueMacro r)

tyLit :: TypeLit -> Expr ctx Ps
tyLit = Term . Literal . TypeLit

constOf :: Expr ctx Ps -> Expr ctx Ps
constOf e = TyApp Const (e ::: VNil)

ptrOf :: Expr ctx Ps -> Expr ctx Ps
ptrOf e = TyApp Pointer (e ::: VNil)

-- | Construct an integer literal expression with a 'signed int' type hint.
-- Suitable for tests where the exact inferred integer type is not the subject
-- under test.
intLit :: Integer -> Expr ctx Ps
intLit n = Term $ Literal $ ValueLit $ ValueInt $
    IntegerLiteral
      (Runtime.Int Runtime.Signed)
      n

add :: Expr ctx Ps -> Expr ctx Ps -> Expr ctx Ps
add a b = VaApp NoXApp MAdd (a ::: b ::: VNil)

shiftLeft :: Expr ctx Ps -> Expr ctx Ps -> Expr ctx Ps
shiftLeft a b = VaApp NoXApp MShiftLeft (a ::: b ::: VNil)

mlocal :: Idx ctx -> Expr ctx Ps
mlocal i = Term $ LocalParam i

mvar :: Identifier -> Expr ctx Ps
mvar n = Term $ Var NoXVar (NameOrdinary n) []

mtagged :: Identifier -> TagKind -> Expr ctx Ps
mtagged n t = Term $ Var NoXVar (NameTagged n t) []
