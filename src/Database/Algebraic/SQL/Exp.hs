{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UnicodeSyntax         #-}

-- | Provides the syntax for constructing SQL expressions.
--
-- @since 1.0.0.0

module Database.Algebraic.SQL.Exp
  (  -- * Field Selection
    (.!.)
    -- * Literals
  , literal
    -- * Comparison
  , (.==.), (.<.), (.<=.), (.>.), (.>=.)

    -- * Re-exports
  , Exp(..), Literal(..), BinOp(..)
  )where

import Database.Algebraic.SQL.Exp.AST
import Database.Algebraic.Table.Type
import Database.Algebraic.Table.Selector

-- | Syntax for a selecting a field from a table.
--
-- > myTable :: Table Persons
-- > ...
-- > exp :: Exp Persons Int
-- > exp = myTable .!. age
--
-- @since 1.0.0.0
(.!.) :: Table tbl -> Selector tbl a -> Exp tbl a
(.!.) = Sel

-- | @since 1.0.0.0
literal :: a -> Exp tbl a
literal = Lit . Literal

-- | @since 1.0.0.0
(.==.) :: Exp tbl a -> Exp tbl a -> Exp tbl Bool
(.==.) = BinOp Eq

-- | @since 1.0.0.0
(.<.) :: Exp tbl a -> Exp tbl a -> Exp tbl Bool
(.<.) = BinOp Lt

-- | @since 1.0.0.0
(.<=.) :: Exp tbl a -> Exp tbl a -> Exp tbl Bool
(.<=.) = BinOp LtEq

-- | @since 1.0.0.0
(.>.) :: Exp tbl a -> Exp tbl a -> Exp tbl Bool
(.>.) = BinOp Gt

-- | @since 1.0.0.0
(.>=.) :: Exp tbl a -> Exp tbl a -> Exp tbl Bool
(.>=.) = BinOp GtEq
