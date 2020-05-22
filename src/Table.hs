{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

-- | Provides an intermediate representation for SQL tables and the machinary to
-- generate them from generic record types.
--
-- @since 1.0.0.0

module Table
  ( -- * SQL Tables
    Table
    -- ** Table Lenses
  , tableName, tableColumns
    -- ** Table construction
  , table
  , SelConfig(..)
  ) where

import Control.Carrier.State.Strict
import Data.HashMap.Strict as H hiding (map)
import Data.Proxy
import Data.Text (Text)
import Lens.Micro
import Lens.Micro.TH

import Column
import Column.Attribute
import Table.Selector

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

-- | Syntactic construct used for specifying further properties a table should
-- hold when being generated. See 'table' for a use case.
--
-- @since 1.0.0.0
infix 0 :-
data SelConfig t where
  (:-) :: Selector t a -> Attribute -> SelConfig t

-- | Smart constructor for a table type. Given a table name and a list of column
-- selectors and attributes we would like to append to those columns, build a
-- SQL table accordingly.
--
-- @since 1.0.0.0
table :: ∀ a. Relational a => Text -> [SelConfig a]  -> Table a
table name configs = Table
  { _tableName    = name
  , _tableColumns =
    let buckets = run $ execState empty (bucketConfigs configs)
        columns = reifyColumns $ Proxy @a
        go (ix, c) = c & attributes <>~ (maybe [] id $ H.lookup ix buckets)
    in map go $ zip [(0 :: Int)..] columns
  }

type SelMap = HashMap Int [Attribute]

bucketConfigs :: Has (State SelMap) sig m => [SelConfig a] -> m ()
bucketConfigs []       = return ()
bucketConfigs ((Selector ix :- a) : xs) = do
  modify @SelMap $ insertWith (++) ix [a]
  bucketConfigs xs
