{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TemplateHaskell   #-}

import GHC.Generics

-- | Friend modules
import Attribute
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

main :: IO ()
main = print personTable
