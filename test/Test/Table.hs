{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}

module Test.Table (tests) where

import Lens.Micro
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

import Database.Algebraic.Column
import Database.Algebraic.Table

data Person = Person
  { age        :: Int
  , firstName  :: String
  , middleName :: Maybe String
  , lastName   :: String
  , cash       :: Double
  } deriving Generic

-- | These should compile as is
people :: Table Person
people = table "people"
  [ #age :- primary
  , #age :- primary -- | Double attribution should be removed.
  ]

tests :: TestTree
tests = testGroup "Table tests"
  [
  ]
