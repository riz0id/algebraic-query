{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

-- | Exports twin functions for queries that work under labelled contexts.
--
-- | @since 1.0.0.0

module Control.Effect.Query.Labelled
  (Query, restrict, select) where

import Control.Effect.Labelled

import           Control.Effect.Query.Internal
import qualified Control.Effect.Query          as Q
import           SQL.Exp
import           Table


-- | @since 1.0.0.0
select :: ∀ label tbl sig m. HasLabelled label (Query tbl) sig m => m (Table tbl)
select = runUnderLabel @label Q.select

-- | @since 1.0.0.0
restrict :: ∀ label tbl sig m. HasLabelled label (Query tbl) sig m
         => Exp tbl Bool -> m ()
restrict r = runUnderLabel @label (Q.restrict r)
