module Database.Algebraic.Compiler.Type where

import Data.DList

-- | String based on difference lists. I don't think Queries will ever get so
-- large that it's reasonable to do immediate write outs so a classic DList plus
-- writer combo is sufficent here.
--
-- @since 1.0.0.0
type DString = DList Char
