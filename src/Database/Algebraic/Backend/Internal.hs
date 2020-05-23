{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Database.Algebraic.Backend.Internal
  ( tableCompiler, insertCompiler
  , TableCompileC(..)
  , DString
  ) where

import           Control.Carrier.Reader
import           Control.Carrier.Writer.Strict
import           Data.DList as D
import           Data.List as L
import qualified Data.Text as T
import           Lens.Micro
import           Lens.Micro.TH
import           Prelude as P

import           Database.Algebraic.Column
import           Database.Algebraic.Table

-- | The configuration passed to a 'Table' compliation unit.
--
-- @since 1.0.0.0
data TableCompileC = TableCompileC
  { _tryExists :: Bool
  -- ^ If we find that the table already exists we don't need to do any work.
  }

$(makeLenses ''TableCompileC)


-- | String based on difference lists. I don't ohink Queries will ever get so
-- large that it's reasonable to do immediate write outs so a classic DList plus
-- writer combo is sufficent here.
--
-- @since 1.0.0.0
type DString = DList Char


tableCompiler :: ( Has (Reader (TableCompileC)) sig m
                 , Has (Writer DString)         sig m )
              => Table a -> m ()
tableCompiler table = do
  config <- ask @TableCompileC

  tell @(DString) $ "CREATE TABLE "
  tell @(DString) $ if config^.tryExists
    then "IF NOT EXISTS "
    else ""
  tell @(DString) . fromList . T.unpack $ table^.tableName

  tell @(DString) " ("
  tell @(DString)
    $ D.concat
    . L.intersperse ", "
    $ P.map columnCompiler (table^.tableColumns)
  tell @(DString) ")"

columnCompiler :: Column -> DString
columnCompiler col = fromList . T.unpack $ col^.name

insertCompiler :: Has (Writer DString) sig m
               => Table a -> [a] -> m ()
insertCompiler table values = do
  tell @(DString) "INSERT INTO "
  tell @(DString) . fromList . T.unpack $ table^.tableName
  tell @(DString) " ("
