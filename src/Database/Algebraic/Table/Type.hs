{-# LANGUAGE TemplateHaskell #-}

module Database.Algebraic.Table.Type
  ( -- * SQL Tables
    Table(..)
    -- * Lenses
  , tableName, tableColumns
  ) where

import Data.Text
import Lens.Micro.TH

import Database.Algebraic.Column.Type

-- | The frontend type for a SQL table. These are constructed via the 'table'
-- smart constructor.
--
-- @
-- data Person :: Person { age :: Int, name :: Text } deriving Generic
--
-- myTable :: Table Person
-- myTable = table "PersonDB" [ age :- Primary ]
-- @
--
-- Will generate a table with the columns "age" of type Integer attributed as
-- the primary key, and "name" attributed implicitly with Required.
--
-- @since 1.0.0.0
data Table a = Table
  { _tableName    :: Text
  , _tableColumns :: [Column]
  } deriving Show

$(makeLenses ''Table)
