-- | SQL column attributes

module Database.Algebraic.Column.Attribute
  ( Attribute(..)
  , primary, autoPrimary
  ) where

-- | We do not explicitly export "Required" and "Optional" because attributing
-- columns with these is done at the type level during table generation. Opening
-- up the constructors for these attributes makes it possible for a column to be
-- optional and required through user error. This sort of pathology is gross to
-- filter out and it's not necessary to export these constructors explicitly
--
-- @since 1.0.0.0
data Attribute
  = Primary
  | AutoPrimary
  | Required
  | Optional
  deriving ( Eq
           , Ord  -- ^ Primarily for "Data.Set" operations. @since 1.0.0
           , Show
           )

-- | @since 0.1.0.0
primary :: Attribute
primary = Primary

-- | @since 0.1.0.0
autoPrimary :: Attribute
autoPrimary = AutoPrimary
