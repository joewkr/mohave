{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Format.HDF.LowLevel.Definitions where

import           Data.Int
import           Data.Type.Equality (type(==))
import           Data.Word
import           Foreign.Storable (Storable)
import           GHC.TypeLits (TypeError, ErrorMessage(..))

data HDFData

class Storable a => HDFDataType a

instance HDFDataType Word8

instance HDFDataType Word16

instance HDFDataType Word32

instance HDFDataType Int8

instance HDFDataType Int16

instance HDFDataType Int32

instance HDFDataType Float

instance HDFDataType Double

type family OneOf (a :: k) (xs :: [k]) :: Bool where
  OneOf a xs = OneOfInternal 'False a xs xs

type family OneOfInternal (found :: Bool) (a :: k) (xs :: [k]) (all :: [k]) :: Bool where
  OneOfInternal 'True  _  _        _   = 'True
  OneOfInternal 'False a '[]       all = TypeError ('ShowType a ':<>: 'Text " is not found in " ':<>: 'ShowType all)
  OneOfInternal  res   a (b ': xs) all = OneOfInternal (a == b) a xs all
