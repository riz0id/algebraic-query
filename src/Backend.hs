{-# LANGUAGE TypeApplications #-}

module Backend
  ( -- * Compliation
    compileTable
  ) where

import Control.Carrier.Writer.Strict
import Control.Carrier.Reader
import Data.DList

import Backend.Internal
import Table

compileTable :: Table tbl -> TableCompileC -> String
compileTable table config
  = toList
  . run
  . runReader config
  . (execWriter @DString)
  $ tableCompiler table
