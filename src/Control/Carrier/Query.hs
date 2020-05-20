{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | @since 1.0.0.0

module Control.Carrier.Query
  ( runQuery

    -- * Re-exports
  , module Control.Effect.Query
  ) where

import Control.Algebra

import Control.Effect.Query
import SQL


-- | @since 1.0.0.0
runQuery :: SQL tbl -> QueryC tbl m a -> m (SQL tbl, a)
runQuery s (QueryC runQueryC) = runQueryC s
{-# INLINE[3] runQuery #-}


-- | @since 1.0.0.0
newtype QueryC tbl m a = QueryC (SQL tbl -> m (SQL tbl, a))
  deriving Functor
-- | @since 1.0.0.0

instance Monad m => Applicative (QueryC tbl m) where
  pure x = QueryC (\s -> pure (s, x))
  {-# INLINE pure #-}

  QueryC f <*> QueryC a = QueryC $ \s -> do
    (s', f') <- f s
    (s'', a') <- a s'
    pure (s'', f' a')
  {-# INLINE (<*>) #-}

  m *> k = m >>= const k
  {-# INLINE (*>) #-}

instance Monad m => Monad (QueryC tbl m) where
  QueryC m >>= f =  QueryC $ \ s -> do
    (s', a) <- m s
    runQuery s' (f a)
  {-# INLINE (>>=) #-}

instance (Algebra sig m, Effect sig) => Algebra (Query tbl :+: sig) (QueryC tbl m) where
  alg (L (Get   k)) = QueryC (\ s -> runQuery s (k s))
  alg (L (Put s k)) = QueryC (\ _ -> runQuery s k)
  alg (R other)     = QueryC (\ s -> alg (thread (s, ()) (uncurry runQuery) other))
  {-# INLINE alg #-}
