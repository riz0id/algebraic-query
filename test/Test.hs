{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import GHC.Generics
import GHC.TypeLits

-- | Friend modules
import Column.Attribute
import Control.Effect.Labelled
import Control.Effect.Query.Labelled
import SQL
import SQL.Exp
import Table

data House= House
  { price   :: Int
  , address :: String
  , aptNum  :: Maybe String
  } deriving Generic

homeTable :: Table House
homeTable = table "HomesDB" [ #address :- Primary ]

data Person = Person
  { age  :: Int
  , name :: Maybe String
  } deriving Generic

personTable :: Table Person
personTable = table "PersonDB"
  [ #age  :- Primary
  , #name :- Primary
  ]

myQuery :: ( HasLabelled 1   (Query Person) sig m
           , HasLabelled "2" (Query Person) sig m )
        => m ()
myQuery = do
  persons1 <- select @1   @Person
  persons2 <- select @"2" @Person
  restrict @1   (persons1 .!. #age .<. literal 20)
  restrict @"2" (persons2 .!. #age .<. literal 25)

main :: IO ()
main = print personTable
