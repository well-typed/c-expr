module C.Expr.Syntax.Name (
    Identifier(..)
  , TagKind(..)
  , Name(..)
  ) where

import Data.String
import Data.Text (Text)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

newtype Identifier = Identifier {
      getIdentifier :: Text
    }
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)

-- | Tag kind for elaborated types
data TagKind = TagStruct | TagUnion | TagEnum
  deriving stock (Eq, Ord, Show)

-- | Macro arguments
data Name =
      NameOrdinary Identifier
    | NameTagged   Identifier TagKind

  deriving stock (Show, Eq, Ord)
