{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
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
import GHC.Generics
import Lens.Micro
import Lens.Micro.TH

import Database.Algebraic.Column.Attribute
import Database.Algebraic.Column.Type

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
  gTblCols :: forall sig m. Has (State Cxt) sig m
           => Proxy rep -> m [ Column ]

instance GRelation a => GRelation (C1 c a) where
  gTblCols _ = gTblCols (Proxy @a)

instance GRelation a => GRelation (D1 c a) where
  gTblCols _ = gTblCols (Proxy :: Proxy a)

instance (Selector c, GRelation a) => GRelation (S1 c a) where
  gTblCols _ = do
    cxtName .= case pack (selName (undefined :: S1 c a b)) of
      "" -> Nothing
      n  -> Just n
    gTblCols (Proxy @a)

instance Typeable a => GRelation (K1 i a) where
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
  gTblCols _ =
    let as = gTblCols (Proxy :: Proxy a)
        bs = gTblCols (Proxy :: Proxy b)
    in liftA2 (++) as bs
