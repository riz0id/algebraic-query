{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Database.Algebraic.Compiler.Insert
  ( compileInsert
  ) where

import Data.DList as D
import Data.List as L
import Database.Algebraic.Column
import Database.Algebraic.Compiler.Combinators
import Database.Algebraic.Compiler.Type
import Database.Algebraic.Table.Type
import GHC.Generics
import Prelude as P

-- | "insertCompiler" creates a INSERT INTO from a "Table" specification @table@
-- and a list of values @a@ to insert into the table.
--
-- @since 0.1.0.0
compileInsert :: Relational a => Table a -> [a] -> DString
compileInsert table (sendValues -> values) =
  "INSERT INTO " <> sendTableName table <> " (" <> sendTableColumns table <> ")"
    <> "\nVALUES " <> values

-- | "sendValues" sends a relational type @a@(the record deciding the structure
-- of our database) to a string of SQL value syntax that contains the values
-- from the record type passed.
--
-- @
-- data Person = Person { name :: String, age :: Int } deriving Generic
--
-- people = [ Person "ann" 30, Person "jon" 32, Person "zach" 24 ]
--
-- sendValues person
-- @
-- @
-- VALUES ('ann', 30),
--        ('jon', 32),
--        ('zach, 24');
-- @
--
-- @since 0.1.0.0
sendValues :: Relational a => [a] -> DString
sendValues xs = (dintercalate ",\n       " $ P.map sendValue xs) <> ";"

-- | "sendValue" is a version of "sendValues" that only takes and produces a
-- a single list of SQL expressions.
--
-- @since 0.1.0.0
sendValue :: Relational a => a -> DString
sendValue x = "(" <> (fromList . L.intercalate ", " . gFieldValues $ from x) <> ")"

-- NOTE: The "DList" package has been updated for this
dintercalate :: DList a -> [DList a] -> DList a
dintercalate sep = D.concat . L.intersperse sep
