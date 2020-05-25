{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}

module Main (main) where

import           Data.Text
import           GHC.Generics
import qualified Test.Table
import           Test.Tasty

import Database.Algebraic
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
people = table (pack "people")
  [ #age :- primary
  , #age :- primary -- | Double attribution should be removed.
  ]

main :: IO ()
main = do
  cre <- dbCreate people
  ins <- dbInsert people
    [ Person 20 "kellan"  (Just "")   "oid"    10
    , Person 23 "glow" Nothing        "coil"   10000
    , Person 20 "toto" (Just "eliza") "brandt" 0
    ]

  putStrLn cre
  putStrLn ins

  defaultMain $ testGroup "unit tests"
    [ Test.Table.tests
    ]
