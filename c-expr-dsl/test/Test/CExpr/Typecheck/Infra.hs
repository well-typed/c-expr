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

import Data.Functor.Identity (Identity (runIdentity))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Nat (Nat (..))
import Data.Vec.Lazy (Vec (..))
import DeBruijn (Idx (..))
import Test.Tasty.HUnit

import C.Type qualified as Runtime

import C.Expr.Syntax
import C.Expr.Typecheck
import C.Expr.Util.Panic

import Test.CExpr.Util

type MacDef = (Identifier, Expr Z (Ps ()))

-- | Run 'tcMacros' on a single macro
--
-- Convenience for tests that exercise one macro in isolation; threads no
-- typedef context.
classifyOne ::
     forall ctx.
     Identifier
  -> Vec ctx Identifier
  -> Expr ctx (Ps ())
  -> MacroTcResult Name
classifyOne name params body =
    case Map.toList (runTcMacros [Macro fakeLoc name params body']) of
      ((_, x):_) -> x
      []         -> panicPure "classifyOne: unexpected empty typecheck result"
  where
    body' :: Expr ctx (Ps Name)
    body' = identityResolutionPass body

-- | Typecheck a sequence of nullary macros in order, threading each successful
-- result into the typing environment for later macros to reference.
runTcSeq :: [MacDef] -> Map Identifier (MacroTcResult Name)
runTcSeq defs =
    runTcMacros [Macro fakeLoc nm VNil (identityResolutionPass body) | (nm, body) <- defs]

-- | Shared 'tcMacros' driver for the test helpers: every annotation projects to
-- 'Nothing', so all variable types resolve through the internal 'TypeEnv'.
runTcMacros :: Show ann => [Macro ann] -> Map Identifier (MacroTcResult ann)
runTcMacros macros = tcMacros (const Nothing) macros

isTypeMacro :: MacroTcResult a -> Bool
isTypeMacro (MacroTcTypeExpr _) = True
isTypeMacro _                   = False

isValueMacro :: MacroTcResult a -> Bool
isValueMacro (MacroTcValueExpr _) = True
isValueMacro _                    = False

assertTypeMacro :: (Show a) => MacroTcResult a -> Assertion
assertTypeMacro r =
    assertBool ("expected MacroTcTypeExpr, got: " ++ show r) (isTypeMacro r)

assertValueMacro :: (Show a) => MacroTcResult a -> Assertion
assertValueMacro r =
    assertBool ("expected MacroTcValueExpr, got: " ++ show r) (isValueMacro r)

tyLit :: TypeLit -> Expr ctx (Ps ())
tyLit = Term . Literal . TypeLit

constOf :: Expr ctx (Ps ()) -> Expr ctx (Ps ())
constOf e = TyApp Const (e ::: VNil)

ptrOf :: Expr ctx (Ps ()) -> Expr ctx (Ps ())
ptrOf e = TyApp Pointer (e ::: VNil)

-- | Construct an integer literal expression with a 'signed int' type hint.
-- Suitable for tests where the exact inferred integer type is not the subject
-- under test.
intLit :: Integer -> Expr ctx (Ps ())
intLit n = Term $ Literal $ ValueLit $ ValueInt $
    IntegerLiteral
      (Runtime.Int Runtime.Signed)
      n

add :: Expr ctx (Ps ()) -> Expr ctx (Ps ()) -> Expr ctx (Ps ())
add a b = VaApp NoXApp MAdd (a ::: b ::: VNil)

shiftLeft :: Expr ctx (Ps ()) -> Expr ctx (Ps ()) -> Expr ctx (Ps ())
shiftLeft a b = VaApp NoXApp MShiftLeft (a ::: b ::: VNil)

mlocal :: Idx ctx -> Expr ctx (Ps ())
mlocal i = Term $ LocalParam i

mvar :: Identifier -> Expr ctx (Ps ())
mvar n = Term $ Var (XVarPs ()) (NameOrdinary n) []

mtagged :: Identifier -> TagKind -> Expr ctx (Ps ())
mtagged n t = Term $ Var (XVarPs ()) (NameTagged n t) []

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Resolve 'Name's to themselves, faking a tiny name resolution pass.
identityResolutionPass :: Expr ctx (Ps a) -> Expr ctx (Ps Name)
identityResolutionPass body = runIdentity $ annotateExpr (\n _ -> pure n) body
