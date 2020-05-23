module Database.Algebraic where

import Control.Effect.Lift

import Database.Algebraic.Table

dbInsert :: Has (Lift IO) sig m => Table a -> [a] -> m Int
dbInsert _     [] = return 0
dbInsert table xs = undefined
