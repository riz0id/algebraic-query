{-# LANGUAGE TemplateHaskell #-}

module Database.Algebraic.Column.Type where

import Data.Set
import Data.Text
import Lens.Micro.TH

import Database.Algebraic.Column.Attribute

-- | 'Column' attributes should not be accessed directly, instead you should use
-- the lenses provided for it.
--
-- FIXME NOTE: Still unclear if access to 'Column' fields here should be granted
-- to the users
--
-- @since 1.0.0.0
data Column = Column
  { _name       :: Text          -- ^ The name of the 'Column'
  , _attributes :: Set Attribute -- ^ SQL attributes attached to this 'Column'
  } deriving (Eq, Show)

makeLenses ''Column
