{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UnicodeSyntax         #-}

module SQL.Exp
  ( Exp(..)
  , Literal(..), literal
  , (.!.), (.==.)
  )where

-- | Friend modules
import SqlSelector
import Table

-- | Expression tree which make up SQL expressions used in queries.
--
-- | @since 1.0.0.0
data Exp tbl a where
  Lit   :: !(Literal tbl a) -> Exp tbl a
  BinOp :: !(BinOp a b) -> Exp tbl a -> Exp tbl a -> Exp tbl b
  Sel   :: Table tbl -> SqlSelector tbl a -> Exp tbl a

-- | Syntax for a selection
--
-- @ since 1.0.0.0
(.!.) :: Table tbl -> SqlSelector tbl a -> Exp tbl a
(.!.) = Sel

-- | Typed literal wrappers in SQL expressions
--
-- | @since 1.0.0.0
newtype Literal tbl a = Literal a

literal :: a -> Exp tbl a
literal = Lit . Literal

-- | Binary operations we support.
--
-- | @since 1.0.0.0
data BinOp a b where
  Eq  :: BinOp a Bool


(.==.) :: ∀ tbl a. Exp tbl a -> Exp tbl a -> Exp tbl Bool
(.==.) = BinOp Eq
