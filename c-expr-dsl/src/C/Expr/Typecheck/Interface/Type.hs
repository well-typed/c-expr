-- | Interface for typechecked macro type expressions
--
-- Intended for qualified import
--
-- @
-- import C.Expr.Typecheck.Interface.Type qualified as T
-- @
module C.Expr.Typecheck.Interface.Type (
    Expr(..)
  , Qual(..)
  , fromExpr
  )
  where

import Control.Exception (Exception)
import Data.Nat (Nat (..))
import Data.Vec.Lazy (Vec (..))
import DeBruijn (idxToInt)

import C.Expr.Syntax qualified as M
import C.Expr.Syntax.Identifier
import C.Expr.Syntax.Name
import C.Expr.Util.Panic

data Expr var =
    TypeLit M.TypeLit
  | Var var
  | App Qual (Expr var)

deriving stock instance Eq var   => Eq         (Expr var)
deriving stock instance Show var => Show       (Expr var)
deriving stock instance             Functor     Expr
deriving stock instance             Foldable    Expr
deriving stock instance             Traversable Expr

-- | Type qualifier
--
-- This is a straight-forward representation of the C syntax. For a detailed
-- discussion of @const@, see
-- https://github.com/well-typed/hs-bindgen/issues/1521.
data Qual =
    Pointer
  | Const
  deriving stock (Eq, Show)

data ConversionError =
    -- | Unexpected value literal (e.g., the integer @42@)
    UnexpectedValueLiteralInType String
    -- | Unexpected named function call in type
  | UnexpectedFunctionCallInType Name
    -- | Unexpected local parameter in type
  | UnexpectedLocalParameterInType Int
    -- | A unary type function received multiple arguments
  | UnexpectedMultipleArgumentsToUnaryTypeFunction
    -- | Unexpected function application on a value (not a type)
  | UnexpectedValueFunctionApplicationInType String
  deriving stock (Show)

instance Exception ConversionError

fromExpr ::
     forall ctx m var p. Applicative m
  => (Identifier -> var)
  -> (M.TagKind -> Identifier -> m var)
  -> M.Expr ctx p
  -> m (Expr var)
fromExpr injectType injectTaggedType = go
  where
    go :: M.Expr ctx p -> m (Expr var)
    go = \case
      M.Term (M.Literal x) ->
        fromLit x
      M.Term (M.LocalParam i) ->
        panicPure $ show $ UnexpectedLocalParameterInType (idxToInt i)
      M.Term (M.Var _ nm []) -> case nm of
        NameOrdinary nm'     -> pure $ Var (injectType nm')
        NameTagged   nm' tag -> Var <$> injectTaggedType tag nm'
      M.Term (M.Var _ nm _ ) ->
        panicPure $ show $ UnexpectedFunctionCallInType nm
      M.TyApp fun args -> do
        let arg = myHead args
        case fun of
          M.Pointer -> App Pointer <$> (go arg)
          M.Const   -> App Const   <$> (go arg)
      M.VaApp _ fun _ ->
        panicPure $ show $ UnexpectedValueFunctionApplicationInType (show fun)

    fromLit :: M.Literal -> m (Expr var)
    fromLit = \case
      M.TypeLit x         -> pure $ TypeLit x
      M.ValueLit x        -> panicPure $ show $ UnexpectedValueLiteralInType (show x)

    myHead :: Vec ('S n) a -> a
    myHead = \case
      (x ::: VNil) ->
        x
      (_ ::: _ ::: _) ->
        panicPure $ show $ UnexpectedMultipleArgumentsToUnaryTypeFunction
