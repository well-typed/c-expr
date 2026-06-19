{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >=908
{-# LANGUAGE TypeAbstractions #-}
#endif

-- | Public entry point for typechecking macros.
module C.Expr.Typecheck (
    tcMacros
  , TypecheckedMacroTypeExpr(..)
  , TypecheckedMacroValueExpr(..)
  , MacroTcResult(..)

    -- * Errors
  , MacroTcError(..)
  , pprMacroTcError
  ) where

import Data.Foldable qualified as Foldable
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Type.Equality ((:~:) (..))
import Data.Type.Nat qualified as Nat
import Data.Vec.Lazy (Vec)
import Data.Vec.Lazy qualified as Vec
import GHC.Generics

import C.Expr.Syntax
import C.Expr.Typecheck.Expr
import C.Expr.Typecheck.Interface.Type qualified as T
import C.Expr.Typecheck.Interface.Value qualified as V
import C.Expr.Typecheck.Type

-- | Batch-typecheck a sequence of macros
--
-- The macros are processed in order. Each successful macro is added to the
-- internal 'TypeEnv' so that subsequent macros can reference it. A macro that
-- fails to typecheck is /not/ added to the environment; later macros that
-- reference it will fail with an unbound-variable error.
--
-- @typeOfAnn@ projects each variable's parse annotation to its type, if known
-- (e.g. for @typedef@ names the embedder supplies); 'Nothing' falls back to the
-- internal 'TypeEnv' of previously-typechecked macros.
tcMacros ::
     forall ann.
     (ann -> Maybe QuantTy)
     -- ^ See the documentation of 'C.Expr.Typecheck.Type.Tc'.
  -> [Macro ann]
  -> Map Identifier (MacroTcResult ann)
tcMacros typeOfAnn macros =
    let (_, tcRs) = Foldable.foldl' step (Map.empty, Map.empty) macros
    in  tcRs
  where
    step ::
         (TypeEnv, Map Identifier (MacroTcResult ann))
      -> Macro ann
      -> (TypeEnv, Map Identifier (MacroTcResult ann))
    step (env, acc) (Macro _loc name params body) =
      let result :: MacroTcResult ann
          result = tcMacroOne typeOfAnn env name params body
          env' = case result of
            MacroTcTypeExpr cmt ->
              Map.insert name (macroTypeType  cmt) env
            MacroTcValueExpr cmv ->
              Map.insert name (macroValueType cmv) env
            MacroTcError _ ->
              env
      in  (env', Map.insert name result acc)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | The macro is a C type expression (e.g., @#define FOO int@).
data TypecheckedMacroTypeExpr ann = TypecheckedMacroTypeExpr{
      macroTypeBody :: T.Expr ann
    , macroTypeType :: Quant (FunValue, Type Ty)
    }
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | The macro is a value expression (e.g., @#define BAR 1@).
data TypecheckedMacroValueExpr ann = forall ctx. TypecheckedMacroValueExpr{
      macroValueParams :: Vec ctx Identifier
    , macroValueBody   :: V.Expr ctx ann
      -- TODO <https://github.com/well-typed/c-expr/issues/8>
      --
      -- We should not require 'FunValue's for value-like expressions.
    , macroValueType   :: Quant (FunValue, Type Ty)
    }
instance Eq ann => Eq (TypecheckedMacroValueExpr ann) where
  (TypecheckedMacroValueExpr @_ @c1 p1 b1 t1) == (TypecheckedMacroValueExpr @_ @c2 p2 b2 t2) =
    t1 == t2 && (
      Vec.withDict p1 $ Vec.withDict p2 $
        case Nat.eqNat @c1 @c2 of
          Just Refl -> p1 == p2 && b1 == b2
          Nothing   -> False
    )
deriving stock instance Show ann => Show (TypecheckedMacroValueExpr ann)
deriving stock instance Functor     TypecheckedMacroValueExpr
deriving stock instance Foldable    TypecheckedMacroValueExpr
deriving stock instance Traversable TypecheckedMacroValueExpr

-- | The result of typechecking a single macro.
data MacroTcResult ann =
    MacroTcTypeExpr    (TypecheckedMacroTypeExpr  ann)
  | MacroTcValueExpr   (TypecheckedMacroValueExpr ann)
  -- | The @c-expr-dsl@ typechecker rejected the macro.
  | MacroTcError       MacroTcError

deriving stock instance (Show ann) => Show (MacroTcResult ann)
deriving stock instance (Eq   ann) => Eq   (MacroTcResult ann)

{-------------------------------------------------------------------------------
  Internal: typecheck a single macro against a given 'TypeEnv'.
-------------------------------------------------------------------------------}

-- | Typecheck a single macro against a given 'TypeEnv'.
tcMacroOne ::
     forall ctx ann.
     (ann -> Maybe QuantTy)
  -> TypeEnv
  -> Identifier
  -> Vec ctx Identifier
  -> Expr ctx (Ps ann)
  -> MacroTcResult ann
tcMacroOne typeOfAnn tyEnv name params expr =
    case tcExpr tyEnv name params (fmapExpr typeOfAnn expr) of
      Left  err -> MacroTcError err
      Right res -> classify res
  where
    classify :: (Type Ty, Quant (FunValue, Type Ty)) -> MacroTcResult ann
    classify = \case
      (MacroTypeTy, quant)
        | not (Vec.null params) ->
          MacroTcError $
            TcUnsupportedTypeWithLocalParameters name (Vec.toList params)
        | otherwise ->
            let texpr :: T.Expr ann
                texpr = T.fromExpr expr
            in if isIncompleteType texpr then
                 MacroTcError $ TcIncompleteTypeMacro name
               else
                 MacroTcTypeExpr $ TypecheckedMacroTypeExpr texpr quant
      (_, quant) ->
        (\vexpr -> MacroTcValueExpr $
          TypecheckedMacroValueExpr params vexpr quant) $
            V.fromExpr expr

    -- | An incomplete type at the top level of a type-like macro: 'void' or
    -- 'const'-wrapped 'void'. Pointer indirection makes the type complete, so
    -- 'void *' (and 'const void *') are not flagged.
    isIncompleteType :: T.Expr var -> Bool
    isIncompleteType = \case
        T.TypeLit TypeVoid -> True
        T.App T.Const e    -> isIncompleteType e
        _                  -> False
