{-# LANGUAGE GADTs #-}

module Database.Algebraic.SQL.Exp.AST
  ( Exp(..), Literal(..), BinOp(..)
  ) where

import Database.Algebraic.Table
import Database.Algebraic.Table.Selector

-- | Expressions used in SQL queries.
--
-- @since 1.0.0.0
data Exp tbl a where
  -- | Expression level literals.
  Lit   :: !(Literal tbl a) -> Exp tbl a

  -- | Binary operations
  BinOp :: !(BinOp a b) -> Exp tbl a -> Exp tbl a -> Exp tbl b

  -- | Selecting a field from a table for use in an expression.
  Sel   :: Table tbl -> Selector tbl a -> Exp tbl a

-- | Typed literal wrappers in SQL expressions
--
-- @since 1.0.0.0
newtype Literal tbl a = Literal a


-- | Supported binary operations.
--
-- @since 1.0.0.0
data BinOp a b where
  Eq   :: BinOp a Bool
  Lt   :: BinOp a Bool
  LtEq :: BinOp a Bool
  Gt   :: BinOp a Bool
  GtEq :: BinOp a Bool
