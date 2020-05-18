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

-- | The 'GRelation' class provides generic instances reifying a witness of a
-- | record type into a list of that record's selector names as well as the SQL
-- | compatible type for that  selector type annotation.
--
-- | @since 1.0.0.0

module Table.Generic (Relational, GRelation, Generic, reifyColumns) where

import Control.Applicative
import Control.Carrier.State.Strict
import Control.Effect.Lens
import Control.Monad
import Lens.Micro
import Lens.Micro.TH
import Data.Text
import Data.Typeable
import GHC.Generics

import Table.Types

-- Carrying indicies for alternative column names.
data Cxt = Cxt
  { _idx  :: Int
  , _name :: Maybe Text
  }

makeLenses ''Cxt


-- | 'reifyColumns' generates column names and types given a record pass as a
-- | type level kind via the proxy. The state computation admits an index for
-- | column names to fall back on in the cases that a record is not provided.
--
-- | In the case we find a datatype with unnamed fields (such as the example
-- | below) we the columns will be named col_1, col_2, ..., col_n where n is the
-- | amount of unnamed datatype selectors.
--
-- | @since 1.0.0.0
reifyColumns :: forall rep. (Relational rep) => Proxy rep -> [ ColInfo ]
reifyColumns _ = join $ evalState cxt (gTblCols $ Proxy @(Rep rep))
  where cxt = Cxt 0 Nothing

-- | The kind of constraint we impose on data types we'll be able to build a
-- | database out of
--
-- | @since 1.0.0.0
type Relational a =
  ( Generic a
  , GRelation (Rep a)
  )

-- | @since 1.0.0.0
class GRelation rep where
  gTblCols :: forall sig m. Has (State Cxt) sig m
           => Proxy rep -> m [ ColInfo ]

instance GRelation a => GRelation (C1 c a) where
  gTblCols _ = gTblCols (Proxy @a)

instance GRelation a => GRelation (D1 c a) where
  gTblCols _ = gTblCols (Proxy :: Proxy a)

instance (Selector c, GRelation a) => GRelation (S1 c a) where
  gTblCols _ = do
    name .= case pack (selName (undefined :: S1 c a b)) of
      "" -> Nothing
      n  -> Just n
    gTblCols (Proxy @a)

instance Typeable a => GRelation (K1 i a) where
  gTblCols _ = do
    st  <- get @ Cxt
    idx += 1
    return $ pure ColInfo
      { colName  = case st^.name of
          Just name' -> name'
          Nothing    -> "col_" <> pack (show $ st^.idx)
      , colAttrs = if proxyTyCon == maybeTyCon
        then [ Optional ]
        else [ Required ]
      }
    where maybeTyCon = typeRepTyCon . typeRep $ Proxy @(Maybe ())
          proxyTyCon = typeRepTyCon . typeRep $ Proxy @a

instance (GRelation a, GRelation b) => GRelation (a :*: b) where
  gTblCols _ =
    let as = gTblCols (Proxy :: Proxy a)
        bs = gTblCols (Proxy :: Proxy b)
    in liftA2 (++) as bs
