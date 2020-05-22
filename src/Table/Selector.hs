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
-- @since 1.0.0.0

-- NOTE: Anywhere in this file where a type variable is named 'n' should be
--   interpreted as TypeLits.Symbol referring to a record field, and 'x' the
--   the recor that field is in.

module Table.Selector
  ( Selector (..), field
    -- *
  , HasField, FieldType, GFieldType, GRSel

  ) where

import Data.Kind
import Data.Proxy
import GHC.Generics         hiding (Selector)
import GHC.OverloadedLabels
import GHC.TypeLits


-- | SqlSelector is a index decorated with the @x@(the record the index belongs
-- to) and @a@(the type of the field that indexing record @x@ will yield).
--
-- See 'field' for more information on the machinary here
--
-- @since 1.0.0.0
newtype Selector x a = Selector { sqlSelectorIx :: Int }
  deriving Show

-- | ∀ (n :: Symbol) (x :: Type), we can produce a index. This index corresponds
-- to the one of the columns that inhabit a table @Table n@ type. Because we
--
-- @since 1.0.0.0
field :: ∀ n x. (HasField n x) => Selector x (FieldType n x)
field = Selector $ case gSel (Proxy @n) (Proxy @ (Rep x)) 0 of
  Left  n -> n
  Right _ -> error "at 'field' - unreachable case breached"
  -- FIXME: This is the condition where the Symbol type argument passed in does
  --   not refer to one of the selectors fields.

-- | Orphan instance that binding a column index from a given field.
--
-- @since 1.0.0.0
instance ( HasField n x
         , FieldType n x ~ a )
  => IsLabel n (Selector x a) where
  fromLabel = field @n @x

-- | Generically nab the of a record selector given a Symbol with the same name.
-- FIXME: Note that this fails if the Symbol does not refer to an actualy
-- field in the record.
--
-- @since 1.0.0.0
type FieldType n x = GFieldType (Rep x) n

-- | Retrieves the type of a record's selector.
--
-- @since 1.0.0.0
type family GetFieldType (f :: Type -> Type) :: Type where
  GetFieldType (M1 _ _ f) = GetFieldType f
  GetFieldType (K1 _ a)   = a

-- | Generically traverses a record's AST provided the type of a record selector
-- matching a provided symbol @n@.
--
-- @since 1.0.0.0
type family GFieldType (a :: Type -> Type) (n :: Symbol) :: Type where
  GFieldType (M1 S ('MetaSel ('Just _) _ _ _) f) _ = GetFieldType f
  GFieldType (M1 _ _ a) n = GFieldType a n
  GFieldType (a :*: _)  n = GFieldType a n

-- | Given a Symbol referring to a record selector, this class provides the
-- functionality to get a index of that selectors position, almost as if the
-- record was an array.
-- FIXME:  Note that this fails if the Symbol does not refer to an actualy
--   field in the record.
--
-- @since 1.0.0.0
class GRSel n (Rep x) => HasField (n :: Symbol) x

-- | @since 1.0.0.0
instance GRSel n (Rep x) => HasField (n :: Symbol) x

-- | Generically get the index of a record's field.
--
-- @since 1.0.0.0
class GRSel (s :: Symbol) (f :: Type -> Type) where
  gSel :: Proxy s -> Proxy f -> Int -> Either Int Int

instance GRSel n (S1 ('MetaSel ('Just n) su ss ds) f) where
  gSel _ _ = Left

instance {-# OVERLAPPABLE #-} GRSel n f => GRSel n (M1 i s f) where
  gSel _ _ = gSel (Proxy @ n) (Proxy @ f)

instance (GRSel n a, GRSel n b) => GRSel n (a :*: b) where
  gSel _ _ n = gSel (Proxy @ n) (Proxy @ a) n
    >>= gSel (Proxy @ n) (Proxy @ b) . succ

instance GRSel name (K1 i a) where
  gSel _ _ = Right
