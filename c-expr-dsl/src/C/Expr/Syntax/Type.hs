-- | AST for C types as they appear in macro definitions
--
-- This covers the minimum amount of C type syntax needed for @hs-bindgen@:
-- primitive types with sign/size specifiers and references to named types
-- (typedefs / other macro types). Const qualifiers and pointer indirection
-- are represented as 'C.Expr.Syntax.Expr.TyApp' nodes in the expression tree
-- using 'C.Expr.Syntax.Expr.Const' and 'C.Expr.Syntax.Expr.Pointer'.
module C.Expr.Syntax.Type (
    TypeLit(..)
  , Sign(..)
  , IntSize(..)
  , FloatSize(..)
  ) where

import GHC.Generics

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | A C type literal as it appears in a macro definition body.
--
-- This is the base type, without const qualifiers or pointer indirections.
-- Those are represented by 'C.Expr.Syntax.Expr.TyApp' nodes wrapping this
-- term in the expression tree.
--
-- Examples:
--
-- > int           => TypeInt Nothing (Just SizeInt)
-- > unsigned long => TypeInt (Just Unsigned) (Just SizeLong)
--
-- Named types (typedefs, type macros) and tagged types (@struct@\/@union@\/@enum@)
-- are not represented here; both parse as 'C.Expr.Syntax.Expr.Var' nodes in
-- the expression layer, and the typechecker decides what they denote.
data TypeLit =
    -- | An integral type: @[signed|unsigned] [short|int|long|long long]@
    --
    -- Both sign and size can be omitted:
    --
    --   * @signed@ alone means @signed int@
    --   * @unsigned@ alone means @unsigned int@
    --   * @short@ alone means @signed short int@
    --   * etc.
    TypeInt !(Maybe Sign) !(Maybe IntSize)

    -- | @[signed|unsigned] char@
  | TypeChar !(Maybe Sign)

    -- | A floating-point type: @float@ or @double@
  | TypeFloat !FloatSize

    -- | @void@
  | TypeVoid

    -- | @_Bool@ or @bool@ (C23)
  | TypeBool
  deriving stock (Eq, Ord, Show, Generic)

data Sign = Signed | Unsigned
  deriving stock (Eq, Ord, Show, Generic)

data IntSize =
    SizeShort      -- ^ @short [int]@
  | SizeInt        -- ^ @int@
  | SizeLong       -- ^ @long [int]@
  | SizeLongLong   -- ^ @long long [int]@
  deriving stock (Eq, Ord, Show, Generic)

data FloatSize =
    SizeFloat    -- ^ @float@
  | SizeDouble   -- ^ @double@
  deriving stock (Eq, Ord, Show, Generic)
