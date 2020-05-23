{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Database.Algebraic.Backend.SQLite where

import Control.Effect.Lift
import Control.Effect.Exception
import Data.Text
import Database.SQLite3
import System.Directory

data Connection = Connection
  { connection :: Database
  , identifier :: Text
  }

openSQLite :: forall e sig m. (Exception e, Has (Lift IO) sig m)
           => FilePath -> m Connection
openSQLite fp = do
  db <- try @e . sendIO . open . pack $ fp
  case db of
    Left  e   -> throwIO e
    Right db' -> do
      absFile <- sendIO (pack <$> makeAbsolute fp)
      return (Connection db' absFile)
