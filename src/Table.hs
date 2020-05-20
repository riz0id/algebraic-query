{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Table (Table, SelConfig(..), table) where

import Control.Carrier.State.Strict
import Data.Functor.Identity
import Data.HashMap.Strict as H hiding (map)
import Data.Proxy
import Data.Text (Text)
import Lens.Micro

-- | Friend modules
import Column
import Column.Attribute
import SqlSelector

-- | The frontend type for a SQL table. These are constructed via the 'table'
-- | smart constructor.
-- | @tableName@    - The name of the table SQL will build.
-- | @tableColumns@ - the columns of the table SQL will build.
--
-- > data Person :: Person { age :: Int, name :: Text } deriving Generic
-- >
-- > myTable :: Table Person
-- > myTable = table "PersonDB" [ age :- Primary ]
-- >
--
-- | Will generate a table with the columns "age" of type Integer attributed as
-- | the primary key, and "name" attributed implicitly with Required.
--
-- | @since 1.0.0.0
data Table a = Table
  { tableName    :: Text
  , tableColumns :: [Column]
  } deriving Show

-- | Syntactic construct used for specifying further properties a table should
-- | hold when being generated. See 'table' for a use case.
--
-- | @since 1.0.0.0
infix 0 :-
data SelConfig t where
  (:-) :: SqlSelector t a -> Attribute -> SelConfig t

-- | Smart constructor for a table type. Given a table name and a list of column
-- | selectors and attributes we would like to append to those columns, build a
-- | SQL table accordingly.
--
-- | @since 1.0.0.0
table :: ∀ a. Relational a => Text -> [SelConfig a]  -> Table a
table name configs = Table
  { tableName    = name
  , tableColumns =
    let buckets = runIdentity $ execState empty (bucketConfigs configs)
        columns = reifyColumns $ Proxy @a
        go (ix, c) = c & attributes <>~ (maybe [] id $ H.lookup ix buckets)
    in map go $ zip [(0 :: Int)..] columns
  }

type SelMap = HashMap Int [Attribute]

bucketConfigs :: Has (State SelMap) sig m => [SelConfig a] -> m ()
bucketConfigs []       = return ()
bucketConfigs ((SqlSelector ix :- a) : xs) = do
  modify @SelMap $ insertWith (++) ix [a]
  bucketConfigs xs
