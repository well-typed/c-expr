module C.Expr.Syntax.TTG.Parse (
    Ps
  , XApp(..)
  , XVar(..)
  ) where

import Data.Kind qualified as Hs
import GHC.Generics (Generic)

import C.Expr.Syntax.TTG

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | The parse pass.
--
-- 'Ps' is parameterised by an annotation type @ann@ (attached to each 'XVar'),
-- so the embedding application can thread its own per-variable data through the
-- parsed tree.
type Ps :: Hs.Type -> Pass
data Ps ann a

{-------------------------------------------------------------------------------
  Pass-indexed type families
-------------------------------------------------------------------------------}

data instance XApp (Ps ann) = NoXApp deriving stock ( Eq, Ord, Show, Generic )
data instance XVar (Ps ann) = XVarPs {
    psAnn :: ann
  }
  deriving stock ( Eq, Ord, Show, Generic )
