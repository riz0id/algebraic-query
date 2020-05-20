{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import GHC.Generics

-- | Friend modules
import Control.Algebra
import Column.Attribute
import Control.Effect.Query
import SQL.Exp
import Table

data Person = Person
  { age  :: Int
  , name :: Maybe String
  } deriving Generic

personTable :: Table Person
personTable = table "PersonDB"
  [ #age  :- Primary
  , #name :- Primary
  ]

myQuery :: Has (Query Person) sig m => m ()
myQuery = do
  restrict (personTable .!. #age .==. literal 1)

main :: IO ()
main = print personTable
