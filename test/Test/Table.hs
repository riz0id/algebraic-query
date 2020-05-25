{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}

module Test.Table (tests) where

import Data.Functor.Identity
import Data.Text
import GHC.Generics
import Lens.Micro
import Test.Tasty
import Test.Tasty.HUnit

import Database.Algebraic
import Database.Algebraic.Column
import Database.Algebraic.Table

tests :: TestTree
tests = testGroup "Table tests"
  [
  ]
