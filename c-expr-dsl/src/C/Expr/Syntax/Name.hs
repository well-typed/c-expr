module C.Expr.Syntax.Name (
    TagKind(..)
  , Name(..)
  ) where

import C.Expr.Syntax.Identifier

-- | Tag kind for elaborated types
data TagKind = TagStruct | TagUnion | TagEnum
  deriving stock (Eq, Ord, Show)

data Name =
    NameOrdinary Identifier
  | NameTagged   Identifier TagKind
  deriving stock (Eq, Ord, Show)
