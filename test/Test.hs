module Main (main) where

import qualified Test.Table
import           Test.Tasty

main :: IO ()
main =  defaultMain $ testGroup "unit tests"
  [ Test.Table.tests
  ]
