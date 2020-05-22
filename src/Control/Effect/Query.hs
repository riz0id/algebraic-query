{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Control.Effect.Query
  ( Query(..), select, restrict
  ) where

import Control.Algebra
import Lens.Micro

import Control.Effect.Query.Internal
import Database.Algebraic.SQL
import Database.Algebraic.SQL.Exp
import Database.Algebraic.Table

-- | Extract a table from a given Query context.
--
-- @since 1.0.0.0
select :: Has (Query tbl) sig m => m (Table tbl)
select = get >>= \sql -> return (sql^.source)
{-# INLINEABLE select #-}

-- | Restrict the query's result by some boolean expression in a given context.
--
-- @since 1.0.0.0
restrict :: Has (Query tbl) sig m => Exp tbl Bool -> m ()
restrict r = modify (\x -> x & restricts .~ (r : x^.restricts))
{-# INLINEABLE restrict #-}
