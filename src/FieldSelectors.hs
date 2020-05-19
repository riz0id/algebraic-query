{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- Disable warning for 'IsLabel' instance.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This field provides an instance for 'IsLabel' for constraining table
-- | attribute lists such that only record selectors can be passed in.
--
-- | NOTE: The GHC extension {-# LANGUAGE OverloadedRecordFields #-} will
-- |   provide the machinery for this constraint but it won't be released till
-- |   some time in the future.

module FieldSelectors
  ( FieldType
  , HasField
  , IsLabel
  ) where

import Data.Kind
import GHC.Generics
import GHC.TypeLits
import GHC.OverloadedLabels

-- | Friend Modules
import Table.Generic
import Selectors

-- | Generic ophan instance for IsLabel
instance ( HasField name t
         , FieldType name t ~ a
         )
  => IsLabel name (SqlSelector t a) where
  fromLabel = field @name @t

-- | Telescope down meta-information to isolate the type annotation given by a
-- | selector field
--
-- | @since 1.0.0.0
type family GetFieldType (f :: Type -> Type) :: Type where
  GetFieldType (M1 _ _ f) = GetFieldType f
  GetFieldType (K1 _ a)   = a

-- | Helper type function to isolate record fields over a generic type.
--
-- | @since 1.0.0.0
type family GFieldType (a :: Type -> Type) (name :: Symbol) :: Type where
  GFieldType (M1 S ('MetaSel ('Just name) _ _ _) f) name = GetFieldType f
  GFieldType (M1 D _ a) name = GFieldType a name
  GFieldType (M1 C _ a) name = GFieldType a name
  GFieldType (a :*: _)  name = GFieldType a name

-- | Given the name of a record selector @name@, and it's record @rec@ we
-- | produce the type of at that field.
--
-- | @since 1.0.0.0
type FieldType name rep = GFieldType (Rep rep) name

-- | NOTE: This patch is pending for the canonical HasField class in future
-- |   versions of GHC.
class ( Relational t
      , GRSel name (Rep t)
      )
  => HasField (name :: Symbol) t

instance ( Relational t
         , GRSel name (Rep t)
         )
  => HasField (name :: Symbol) t

field :: forall name t. (HasField name t) => SqlSelector t (FieldType name t)
field = SqlSelector $ gSel @name @(Rep t) 0

class GRSel (s :: Symbol) (f :: Type -> Type) where
  gSel :: Int -> Int

instance GRSel name (M1 S ('MetaSel ('Just name) su ss ds) f) where
  gSel n = n

instance GRSel name f => GRSel name (M1 C s f) where
  gSel = gSel @name @f

instance GRSel name f => GRSel name (M1 D s f) where
  gSel = gSel @name @f

instance GRSel name a => GRSel name (a :*: b) where
  gSel n = gSel @name @a n
