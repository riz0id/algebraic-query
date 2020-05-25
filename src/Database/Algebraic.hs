{-# LANGUAGE FlexibleContexts #-}

module Database.Algebraic
  ( -- * Operations
    dbCreate
  , dbInsert
    -- * Columns
  , Column

  ) where

import Control.Carrier.Reader
import Control.Effect.Lift
import Data.DList as D
import Database.Algebraic.Column
import Database.Algebraic.Compiler.Create
import Database.Algebraic.Compiler.Insert
import Database.Algebraic.Table

dbCreate :: Has (Lift IO) sig m => Table a -> m String
dbCreate = fmap toList . runReader (TableCompile False) . tableCompiler

dbInsert :: (Relational a, Has (Lift IO) sig m) => Table a -> [a] -> m String
dbInsert _     [] = return "nothin"
dbInsert table xs = return . D.toList $ compileInsert table xs
