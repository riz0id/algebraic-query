{-# LANGUAGE TypeApplications #-}

module Database.Algebraic.Backend
  ( -- * Compliation
    compileTable
  ) where

import Control.Carrier.Writer.Strict
import Control.Carrier.Reader
import Data.DList

import Database.Algebraic.Backend.Internal
import Database.Algebraic.Table

compileTable :: Table tbl -> TableCompileC -> String
compileTable table config
  = toList
  . run
  . runReader config
  . (execWriter @DString)
  $ tableCompiler table
