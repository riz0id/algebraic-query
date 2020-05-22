{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Control.Effect.Query.Internal
  (Query(..), get, put, modify) where

import Control.Algebra
import GHC.Generics

import Database.Algebraic.SQL

-- | @since 1.0.0.0
data Query tbl m k
  = Get (SQL tbl -> m k)
  | Put (SQL tbl) (m k)
  deriving (Functor, Generic1)

instance HFunctor (Query tbl)
instance Effect   (Query tbl)

-- | @since 1.0.0.0
get :: Has (Query tbl) sig m => m (SQL tbl)
get = send (Get pure)
{-# INLINEABLE get #-}

-- | @since 1.0.0.0
put :: Has (Query tbl) sig m => SQL tbl -> m ()
put s = send (Put s (pure ()))
{-# INLINEABLE put #-}

-- | @since 1.0.0.0
modify :: Has (Query tbl) sig m => (SQL tbl -> SQL tbl) -> m ()
modify f = do
  a <- get
  put $! f a
{-# INLINEABLE modify #-}
