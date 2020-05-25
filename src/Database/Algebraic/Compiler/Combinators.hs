{-# LANGUAGE OverloadedStrings #-}

module Database.Algebraic.Compiler.Combinators
  ( sendTableName, sendTableColumns
  ) where

import Data.DList as D
import Data.List as L
import Data.Text
import Database.Algebraic.Column
import Database.Algebraic.Compiler.Type
import Database.Algebraic.Table.Type

-- | "sendTableName" is just a convince function for sending a "Table" to a
-- it's table name as a "DString".
--
-- @since 0.1.0.0
sendTableName :: Table a -> DString
sendTableName = fromList . unpack . _tableName

-- | "sendColumns" consumes a "Table" and produces the a list of that table's
-- column names enclosed in parenthesis.
--
-- @
-- data Person = P { age :: Int, name :: String } deriving Generic
--
-- person = P 32 "This value doesn't matter in the example."
--
-- main = print $ sendTableColumns P
-- @
-- >>> (age, name)
--
-- @since 0.1.0.0
sendTableColumns :: Table a -> DString
sendTableColumns = dintercalate ", "
                 . L.map (fromList . unpack . _name)
                 . _tableColumns

-- NOTE: The "DList" package has been updated for this
dintercalate :: DList a -> [DList a] -> DList a
dintercalate sep = D.concat . L.intersperse sep
