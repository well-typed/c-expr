{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >=908
{-# LANGUAGE TypeAbstractions #-}
#endif

-- | The syntax for macros recognized
--
-- Intended for unqualified import.
module C.Expr.Syntax (
    -- * Definition
    Macro(..)
    -- ** Type syntax
  , TypeLit(..)
  , TagKind(..)
  , Sign(..)
  , IntSize(..)
  , FloatSize(..)
    -- ** Expressions
  , Name(..)
  , Expr(..)
  , TyQual(..)
  , VaFun(..)
  , ValueLit(..)
  , Literal(..)
  , Term(..)
    -- ** Literals
  , IntegerLiteral(..)
  , FloatingLiteral(..)
  , CharLiteral(..)
  , StringLiteral(..)
  , canBeRepresentedAsRational
    -- ** Annotations
  , Pass
  , Ps
  , XVar(..)
  , XApp(..)
  ) where

import Data.Kind qualified as Hs
import Data.Type.Equality ((:~:) (..))
import Data.Type.Nat qualified as Nat
import Data.Vec.Lazy (Vec, withDict)
import DeBruijn (Ctx)

import C.Expr.Syntax.Expr
import C.Expr.Syntax.Literal
import C.Expr.Syntax.Name
import C.Expr.Syntax.TTG
import C.Expr.Syntax.TTG.Parse
import C.Expr.Syntax.Type

import Clang.HighLevel.Types

type Macro :: Hs.Type -> Hs.Type
data Macro var = forall (ctx :: Ctx). Macro {
      macroLoc    :: MultiLoc
    , macroName   :: Name
    , macroParams :: Vec ctx Name
    , macroExpr   :: Expr var ctx Ps
    }

instance Eq var => Eq (Macro var) where
  (Macro @_ @c1 loc1 n1 p1 e1) == (Macro @_ @c2 loc2 n2 p2 e2) =
      loc1 == loc2 && n1   == n2 && eqBody
    where
      eqBody = withDict p1 $ withDict p2 $
        case Nat.eqNat @c1 @c2 of
          Just Refl -> p1 == p2 && e1 == e2
          Nothing   -> False

deriving stock instance (Show var) => Show (Macro var)

instance Functor Macro where
  fmap f Macro{macroLoc, macroName, macroParams, macroExpr} =
    Macro {
        macroLoc
      , macroName
      , macroParams
      , macroExpr = mapExprVar f macroExpr
      }
