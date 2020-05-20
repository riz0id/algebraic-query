{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax   #-}

-- | @since 1.0.0.0

module SQL (SQL, source, columns, restricts) where

import Lens.Micro.TH

import SQL.Exp
import Table

-- | The internal state held by a 'Query' context.
--
-- @since 1.0.0.02
data SQL tbl = SQL
  { _source    :: Table tbl
  , _columns   :: ∀ a. [Exp tbl a]
  , _restricts :: ![Exp tbl Bool]
  }

$(makeLenses ''SQL)
