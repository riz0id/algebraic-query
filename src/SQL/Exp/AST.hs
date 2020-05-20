{-# LANGUAGE GADTs #-}

module SQL.Exp.AST ( Exp(..), Literal(..), BinOp(..) ) where

import Selector
import Table

-- | Expression tree which make up SQL expressions used in queries.
--
-- | @since 1.0.0.0
data Exp tbl a where
  Lit   :: !(Literal tbl a) -> Exp tbl a
  BinOp :: !(BinOp a b) -> Exp tbl a -> Exp tbl a -> Exp tbl b
  Sel   :: Table tbl -> Selector tbl a -> Exp tbl a


-- | Typed literal wrappers in SQL expressions
--
-- | @since 1.0.0.0
newtype Literal tbl a = Literal a


-- | Binary operations we support.
--
-- | @since 1.0.0.0
data BinOp a b where
  Eq   :: BinOp a Bool
  Lt   :: BinOp a Bool
  LtEq :: BinOp a Bool
  Gt   :: BinOp a Bool
  GtEq :: BinOp a Bool
