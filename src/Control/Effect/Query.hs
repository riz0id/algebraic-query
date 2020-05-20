{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Control.Effect.Query (Query(..), restrict) where

import Control.Algebra
import GHC.Generics (Generic1)
import Lens.Micro

-- | Friend modules
import SQL
import SQL.Exp

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

-- | @since 1.0.0.0
restrict :: Has (Query tbl) sig m => Exp tbl Bool -> m ()
restrict r = modify (\x -> x & restricts .~ (r : x^.restricts))
{-# INLINEABLE restrict #-}
