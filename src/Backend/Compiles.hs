{-# LANGUAGE #-}

module Compiles where

import Data.Kind

class Compiles a where
  type Config a :: Type
