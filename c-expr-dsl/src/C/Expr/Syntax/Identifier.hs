module C.Expr.Syntax.Identifier (
    Identifier(..)
  ) where

import Data.String
import Data.Text (Text)
import GHC.Generics (Generic)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | A C identifier
--
-- Used for any name in macro source: macro parameters, free variables, typedef
-- names, and the identifier part of a tagged type (e.g. the @Foo@ in
-- @struct Foo@).
newtype Identifier = Identifier {
      getIdentifier :: Text
    }
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)
  deriving stock (Generic)
