{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Database.Algebraic.Compiler.Create
  ( TableCompile(..)
  , tableCompiler
  ) where

import Control.Effect.Reader
import Database.Algebraic.Compiler.Combinators
import Database.Algebraic.Compiler.Type
import Database.Algebraic.Table.Type

data TableCompile = TableCompile
  { tryExists :: Bool
  }

-- | "tableCompiler" consumes a "Table" and produces a CREATE SQL statement.
--
-- @since 0.1.0.0
tableCompiler :: Has (Reader TableCompile) sig m => Table a -> m DString
tableCompiler table = do
  ifEx <- ask @TableCompile >>= sendIfEx
  return $ "CREATE TABLE " <> ifEx <> sendTableName table
    <> " (" <> sendTableColumns table <> ")"

-- | "sendIFEx" takes in the argument "tryExists" from a "TableCompile" and
-- produces a SQL "IF NOT EXISTS" statement if it's true or a empty "DList" if
-- not.
--
-- @since 0.1.0.0
sendIfEx :: Monad m => TableCompile -> m DString
sendIfEx options = if tryExists options
  then return "IF NOT EXISTS "
  else return mempty
