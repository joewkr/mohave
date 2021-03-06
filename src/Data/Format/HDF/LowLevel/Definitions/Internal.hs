{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Format.HDF.LowLevel.Definitions.Internal where

import qualified Data.ByteString as BS
import           Data.ByteString.Unsafe (unsafeUseAsCString)
import           Data.Int
import           Data.Kind
import           Data.Type.Equality (type(==))
import qualified Data.Vector.Storable as VS
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable (Storable)
import           GHC.TypeLits (TypeError, ErrorMessage(..))


import           Data.Format.HDF.LowLevel.Definitions

class SDObjectId id where
    getRawObjectId :: id -> Int32

type family IsElementOf (a :: Type) (t :: Type) :: Constraint where
    IsElementOf a [b] = a ~ b
    IsElementOf a (VS.Vector b) = a ~ b
    IsElementOf a BS.ByteString = a `OneOf` '[Char8, UChar8]

class CanBeAttribute t where
    withAttributePtr :: t -> (Ptr HDFData -> IO b) -> IO b
    attributeLen :: t -> Int

instance Storable a => CanBeAttribute [a] where
    withAttributePtr val f = withArray val (\ptr -> f $! castPtr ptr)
    attributeLen = length

instance CanBeAttribute BS.ByteString where
    withAttributePtr val f = unsafeUseAsCString val (\ptr -> f $! castPtr ptr)
    attributeLen = BS.length

instance Storable a =>  CanBeAttribute (VS.Vector a) where
    withAttributePtr val f = VS.unsafeWith val (\ptr -> f $! castPtr ptr)
    attributeLen = VS.length

type family OneOf (a :: k) (xs :: [k]) :: Constraint where
  OneOf a xs = (OneOfInternal 'False a xs xs) ~ 'True

type family OneOfInternal (found :: Bool) (a :: k) (xs :: [k]) (all :: [k]) :: Bool where
  OneOfInternal 'True  _  _        _   = 'True
  OneOfInternal 'False a '[]       all = TypeError ('ShowType a ':<>: 'Text " is not found in " ':<>: 'ShowType all)
  OneOfInternal  res   a (b ': xs) all = OneOfInternal (a == b) a xs all
