{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

-- | The 'GRelation' class provides generic instances reifying a witness of a
-- record type into a list of that record's selector names as well as the SQL
-- compatible type for that  selector type annotation.
--
-- @since 1.0.0.0

module Database.Algebraic.Column.Generic
  ( Relational, GRelation(..), Cxt(..), reifyColumns
  ) where

import Control.Applicative
import Control.Carrier.State.Strict
import Control.Effect.Lens
import Data.Proxy
import Data.Set as Set
import Data.Text
import Data.Typeable
import Database.Algebraic.Column.Attribute
import Database.Algebraic.Column.Type
import GHC.Generics
import Lens.Micro
import Lens.Micro.TH

-- Carrying indicies for alternative column names.
data Cxt = Cxt
  { _idx     :: Int
  , _cxtName :: Maybe Text
  }

makeLenses ''Cxt

-- | Reifies all of record @x@'s fields into columns with the same names.
--
-- @since 1.0.0.0
reifyColumns :: ∀ x. Relational x => Proxy x -> [ Column ]
reifyColumns _ = run $ evalState cxt (gTblCols $ Proxy @(Rep x))
  where cxt = Cxt 0 Nothing

-- | Converts a value to a "String" for use in SQL syntax.
--
-- @since 0.1.0.0
class SqlShow a where
  sqlShow :: a -> String

  default sqlShow :: Show a => a -> String
  sqlShow = show

instance SqlShow Double
instance SqlShow Int

instance SqlShow String where
  sqlShow x = "'" ++ x ++ "'"

instance SqlShow a => SqlShow (Maybe a) where
  sqlShow (Just x) = sqlShow x
  sqlShow Nothing  = "NULL"

-- | The kind of constraint we impose on data types we'll be able to build a
-- database out of.
--
-- @since 1.0.0.0
type Relational a =
  ( Generic a
  , GRelation (Rep a)
  )

-- | @since 1.0.0.0
class GRelation rep where
  -- | gFieldValues extracts all the values in a term level record and gives
  -- them back as a heterogeneous list.
  --
  -- @since 0.1.0.0
  gFieldValues :: rep a -> [ String ]

  -- | gTblCols traverses the generic representation of a record @rep@ to
  -- generate columns where the column name is the name of the field
  -- selector.
  --
  -- @since 0.1.0.0
  gTblCols :: forall sig m. Has (State Cxt) sig m => Proxy rep -> m [ Column ]

instance GRelation a => GRelation (C1 c a) where
  gFieldValues (M1 x) = gFieldValues x

  gTblCols _ = gTblCols (Proxy @a)

instance GRelation a => GRelation (D1 c a) where
  gFieldValues (M1 x) = gFieldValues x

  gTblCols _ = gTblCols (Proxy :: Proxy a)

instance (Selector c, GRelation a) => GRelation (S1 c a) where
  gFieldValues (M1 x) = gFieldValues x

  gTblCols _ = do
    cxtName .= case pack (selName (undefined :: S1 c a b)) of
      "" -> Nothing
      n  -> Just n
    gTblCols (Proxy @a)

instance (SqlShow a, Typeable a) => GRelation (K1 i a) where
  gFieldValues (K1 x) = return $! sqlShow x

  gTblCols _ = do
    st  <- get @ Cxt
    idx += 1
    return $ pure Column
      { _name  = case st^.cxtName of
          Just name' -> name'
          Nothing    -> "col_" <> pack (show $ st^.idx)
      , _attributes = if proxyTyCon == maybeTyCon
        then Set.singleton Optional
        else Set.singleton Required
      }
    where maybeTyCon = typeRepTyCon . typeRep $ Proxy @(Maybe ())
          proxyTyCon = typeRepTyCon . typeRep $ Proxy @a

instance (GRelation a, GRelation b) => GRelation (a :*: b) where
  gFieldValues (a :*: b) = (gFieldValues a) ++ (gFieldValues b)

  gTblCols _ =
    let as = gTblCols (Proxy :: Proxy a)
        bs = gTblCols (Proxy :: Proxy b)
    in liftA2 (++) as bs
