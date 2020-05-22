module Database.Algebraic.Column
  ( -- * Columns
    Column(..)
  , name
  , attributes
  , reifyColumns
  , Relational
    -- * Attributes
  , Attribute
  , primary
  , autoPrimary
  ) where

import Database.Algebraic.Column.Attribute
import Database.Algebraic.Column.Generic
import Database.Algebraic.Column.Type (Column(..), name, attributes)
