-- | Interface for typechecked macro value expressions
--
-- Intended for qualified import
--
-- @
-- import C.Expr.Typecheck.Interface.Value qualified as V
-- @
module C.Expr.Typecheck.Interface.Value (
    Expr(..)
  , fromExpr
  )
  where

import Control.Exception (Exception)
import Data.GADT.Compare (GEq (..))
import Data.Nat (Nat (..))
import Data.Type.Equality ((:~:) (..))
import Data.Vec.Lazy (Vec)
import DeBruijn (Idx)

import C.Expr.Syntax qualified as M
import C.Expr.Syntax.TTG.Parse
import C.Expr.Util.Panic

data Expr ctx var =
    Literal M.ValueLit
  | LocalParam (Idx ctx)
  | Var var [Expr ctx var]
  | forall n . App (M.VaFun (S n)) (Vec (S n) (Expr ctx var))

deriving stock instance (Show var) => Show        (Expr ctx var)
deriving stock instance               Functor     (Expr ctx)
deriving stock instance               Foldable    (Expr ctx)
deriving stock instance               Traversable (Expr ctx)

instance Eq var => Eq (Expr ctx var) where
  Literal l1    == Literal l2    = l1 == l2
  LocalParam n1 == LocalParam n2 = n1 == n2
  Var v1 as1    == Var v2 as2    = v1 == v2 && as1 == as2
  App f1 xs1    == App f2 xs2    =
    case f1 `geq` f2 of
      Just Refl -> xs1 == xs2
      Nothing   -> False
  _ == _ = False

data ConversionError =
    -- | Unexpected type in a value expression (e.g., @int@)
    UnexpectedTypeInValue String
    -- | Unexpected function application on a type (not a value)
  | UnexpectedTypeFunctionApplicationInValue String
  deriving stock (Show)

instance Exception ConversionError

-- | Translate into the typechecked AST assuming the expression is a value.
--
-- For variables, we don't use their name but their annotations.
fromExpr ::
     forall ctx ann.
     M.Expr ctx (Ps ann)
  -> Expr ctx ann
fromExpr = go
  where
    go :: M.Expr ctx (Ps ann) -> Expr ctx ann
    go = \case
      M.Term (M.Literal x) ->
        fromLit x
      M.Term (M.LocalParam i) ->
        LocalParam i
      M.Term (M.Var XVarPs{psAnn} _nm args) ->
        Var psAnn (map go args)
      M.TyApp fun _ ->
        panicPure $ show $ UnexpectedTypeFunctionApplicationInValue (show fun)
      M.VaApp _ fun args ->
        App fun $ fmap go args

    fromLit :: M.Literal -> Expr ctx ann
    fromLit = \case
      M.TypeLit x ->
        panicPure $ show $ UnexpectedTypeInValue (show x)
      M.ValueLit x -> Literal $ case x of
        M.ValueInt y    -> M.ValueInt y
        M.ValueFloat y  -> M.ValueFloat y
        M.ValueChar y   -> M.ValueChar y
        M.ValueString y -> M.ValueString y
