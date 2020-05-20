{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Table (Table, SelConfig(..), table) where

import Control.Carrier.State.Strict
import Control.Monad
import Data.Functor.Identity
import Data.HashMap.Strict as H hiding (map)
import Data.Proxy
import Data.Text  (Text)

-- | Friend modules
import Attribute
import Column
import SqlSelector

data Table a = Table
  { tableName    :: Text
  , tableColumns :: [Column]
  } deriving Show

table :: ∀ a. Relational a => Text -> [SelConfig a]  -> Table a
table name configs = Table
  { tableName    = name
  , tableColumns =
    let buckets = runIdentity $ execState empty (bucketConfigs configs)
        columns = reifyColumns $ Proxy @a
        go (ix, c) = c { columnAttributes = addAttrs (H.lookup ix buckets) (columnAttributes c) }
    in map go $ zip [(0 :: Int)..] columns
  }

addAttrs :: Maybe [Attribute] -> [Attribute] -> [Attribute]
addAttrs (Just x) y = x ++ y
addAttrs Nothing  y = y

type SelMap = HashMap Int [Attribute]

bucketConfigs :: Has (State SelMap) sig m => [SelConfig a] -> m ()
bucketConfigs (x : xs) = modify @SelMap $ insertWith (++) (selIndex x) [selAttr x]
bucketConfigs []       = return ()

infix 0 :-
data SelConfig t where
  (:-) :: SqlSelector t a -> Attribute -> SelConfig t

selAttr :: SelConfig t -> Attribute
selAttr (_ :- a) = a

selIndex :: SelConfig t -> Int
selIndex (SqlSelector ix :- _) = ix
