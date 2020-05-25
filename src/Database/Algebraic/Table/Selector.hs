{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE UnicodeSyntax         #-}

-- | This module provides machinery for converting Symbols hopefully (see
-- fixmes) referring to one of the fields of a record into a index for that
-- fields position in the record.
--
-- This is useful for get a corresponding column from the table representation
-- of some record via the name of that column.
--
-- @since 0.1.0.0

module Database.Algebraic.Table.Selector
  ( Selector (..), field
    -- * Lalala
  , HasField, FieldType, GFieldType, GRSel

  ) where

import Data.Kind
import Data.Proxy
import GHC.Generics         hiding (Selector)
import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits

-- | SqlSelector is a index decorated with the @x@(the record the index belongs
-- to) and @a@(the type of the field that indexing record @x@ will yield).
--
-- See 'field' for more information on the machinary here
--
-- @since 0.1.0.0
newtype Selector x a = Selector { sqlSelectorIx :: Int }
  deriving Show

-- | ∀ (n :: Symbol) (x :: Type), we can produce a index. This index corresponds
-- to the one of the columns that inhabit a table @Table n@ type. Because we
--
-- @since 0.1.0.0
field :: ∀ n x a. (HasField n x a, GRSel n (Rep x)) => Selector x a
field = Selector $ case gSelector (Proxy @n) (Proxy @ (Rep x)) 0 of
  Left  n -> n
  Right _ -> error "This is a bug, if you have reached this point please file a issue "

-- | Orphan instance that binding a column index from a given field.
--
-- @since 0.1.0.0
instance (HasField n x a, GRSel n (Rep x)) => IsLabel n (Selector x a) where
  fromLabel = field @n @x

-- | Generically nab the of a record selector given a Symbol with the same name.
--
-- @since 0.1.0.0
type FieldType n x = GFieldType (Rep x) n

-- | Retrieves the type of a record's selector.
--
-- @since 0.1.0.0
type family GetFieldType (f :: Type -> Type) :: Type where
  GetFieldType (M1 _ _ f) = GetFieldType f
  GetFieldType (K1 _ a)   = a

-- | Generically traverses a record's AST provided the type of a record selector
-- matching a provided symbol @n@.
--
-- @since 0.1.0.0
type family GFieldType (a :: Type -> Type) (n :: Symbol) :: Type where
  GFieldType (M1 S ('MetaSel ('Just _) _ _ _) f) _ = GetFieldType f
  GFieldType (M1 _ _ a) n = GFieldType a n
  GFieldType (a :*: _)  n = GFieldType a n

-- | Generic operations over records we use as databases.
--
-- @since 0.1.0.0
class GRSel (s :: Symbol) (f :: Type -> Type) where
  -- | gSelector provides the functionality to find the index of a field @s@
  -- from a record type x where @f@ ~ @(Rep x) as if it were an array.
  --
  -- @since 0.1.0.0
  gSelector :: Proxy s -> Proxy f -> Int -> Either Int Int

instance GRSel n (S1 ('MetaSel ('Just n) su ss ds) f) where
  gSelector _ _ = Left

instance {-# OVERLAPPABLE #-} GRSel n f => GRSel n (M1 i s f) where
  gSelector _ _ = gSelector (Proxy @ n) (Proxy @ f)

instance (GRSel n a, GRSel n b) => GRSel n (a :*: b) where
  gSelector _ _ n = gSelector (Proxy @ n) (Proxy @ a) n
    >>= gSelector (Proxy @ n) (Proxy @ b) . succ

instance GRSel name (K1 i a) where
  gSelector _ _ = Right
