module Database.Algebraic.Column
  ( -- * Columns
    Column(..)
  , name
  , attributes
  , reifyColumns
  , Relational
  , GRelation(..)
    -- * Attributes
  , Attribute
  , primary
  , autoPrimary
  ) where

import Database.Algebraic.Column.Attribute
import Database.Algebraic.Column.Generic
import Database.Algebraic.Column.Type (Column(..), name, attributes)
