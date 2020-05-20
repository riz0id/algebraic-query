{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax   #-}

-- | @since 1.0.0.0

module SQL (SQL, sources, columns, restricts) where

import Lens.Micro.TH

-- | Friend modules
import SQL.Exp
import Table

data SQL tbl = SQL
  { _sources   :: Table tbl
  , _columns   :: ∀ a. [Exp tbl a]
  , _restricts :: ![Exp tbl Bool]
  }

$(makeLenses ''SQL)
