-- | SQL column attributes

module Column.Attribute
  ( Attribute(..)
  ) where

-- | @since 1.0.0.0
data Attribute
  = Primary
  | AutoPrimary
  | Required
  | Optional
  deriving Show
