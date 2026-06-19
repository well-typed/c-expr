module C.Expr.Syntax.Identifier (
    Identifier(..)
  ) where

import Data.String
import Data.Text (Text)
import GHC.Generics (Generic)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Macro arguments
newtype Identifier = Identifier {
      getIdentifier :: Text
    }
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)
  deriving stock (Generic)
