{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Format.NetCDF.LowLevel.Definitions where

import qualified Data.ByteString as BS
import           Data.Int
import           Data.Type.Equality (TestEquality, testEquality, (:~:)(Refl))
import           Data.Word
import           Foreign.C.String (CString)
import           Foreign.C.Types
import           Foreign.Ptr (castPtr)
import           Foreign.Storable (Storable(..))
import           GHC.TypeNats (Nat, KnownNat)

import           Internal.Definitions

data NCData

data FileId
data GroupId

data NCOpenMode =
    NCNoWrite
  | NCWrite
  | NCShare
  | NCDiskless
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,2)
  | NCPersist
#endif
  | NCClobber
  | NCNoClobber
  | NCCompoundMode CInt
  deriving (Eq, Show)

data NCFormat =
    NC64BitOffset
  | NC64BitData
  | NCNetCDF4
  | NCClassic -- Use CDF-1 file format
  | NCClassicModel NCFormat
  deriving (Eq, Show)

data NCFormatX =
    NCFormatXNC3
  | NCFormatXNChdf5
  | NCFormatXNChdf4
  | NCFormatXPNetCDF
  | NCFormatXDAP2
  | NCFormatXDAP4
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,2)
  | NCFormatXUDF0
  | NCFormatXUDF1
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,7,0)
  | NCFormatXZARR
#endif
  | NCFormatXUndefined
  deriving (Eq, Show)

data NCFillMode = NCFill | NCNoFill

data NCStorageType = NCContiguous | NCChunked

data NCEndianness = NCEndianNative | NCEndianLittle | NCEndianBig deriving (Eq, Show)

data NC c where
    NCFile :: CInt -> NC FileId
    NCGroup :: CInt -> NC GroupId

ncRawId :: NC id -> CInt
ncRawId (NCFile ncid) = ncid
ncRawId (NCGroup ncid) = ncid

newtype NCDimensionId = NCDimensionId CInt deriving Eq

instance Storable NCDimensionId where
    sizeOf    (NCDimensionId dimId) = sizeOf dimId
    alignment (NCDimensionId dimId) = alignment dimId
    peek ptr                        = NCDimensionId <$> peek (castPtr ptr)
    poke ptr  (NCDimensionId dimId) = poke (castPtr ptr) dimId

newtype NCStringPtr = NCStringPtr CString deriving (Eq, Show)

instance Storable NCStringPtr where
    sizeOf    (NCStringPtr strPtr) = sizeOf strPtr
    alignment (NCStringPtr strPtr) = alignment strPtr
    peek ptr                        = NCStringPtr <$> peek (castPtr ptr)
    poke ptr  (NCStringPtr strPtr) = poke (castPtr ptr) strPtr

data NCDataType a where
    NCNone   :: NCDataType ()
    NCByte   :: NCDataType Int8
    NCUByte  :: NCDataType Word8
    NCChar   :: NCDataType Int8
    NCShort  :: NCDataType Int16
    NCUShort :: NCDataType Word16
    NCInt    :: NCDataType Int32
    NCUInt   :: NCDataType Word32
    NCInt64  :: NCDataType Int64
    NCUInt64 :: NCDataType Word64
    NCFloat  :: NCDataType Float
    NCDouble :: NCDataType Double
    NCString :: NCDataType NCStringPtr -- When reading from a NetCDF file memory is allocated on the C-side

deriving instance Show (NCDataType a)

instance TestEquality NCDataType where
    testEquality a b = case a of
      NCNone -> case b of
        NCNone -> Just Refl
        _ -> Nothing
      NCByte -> case b of
        NCByte -> Just Refl
        _ -> Nothing
      NCUByte -> case b of
        NCUByte -> Just Refl
        _ -> Nothing
      NCChar -> case b of
        NCChar -> Just Refl
        _ -> Nothing
      NCShort -> case b of
        NCShort -> Just Refl
        _ -> Nothing
      NCUShort -> case b of
        NCUShort -> Just Refl
        _ -> Nothing
      NCInt -> case b of
        NCInt -> Just Refl
        _ -> Nothing
      NCUInt -> case b of
        NCUInt -> Just Refl
        _ -> Nothing
      NCInt64 -> case b of
        NCInt64 -> Just Refl
        _ -> Nothing
      NCUInt64 -> case b of
        NCUInt64 -> Just Refl
        _ -> Nothing
      NCFloat -> case b of
        NCFloat -> Just Refl
        _ -> Nothing
      NCDouble -> case b of
        NCDouble -> Just Refl
        _ -> Nothing
      NCString -> case b of
        NCString -> Just Refl
        _ -> Nothing

newtype NCVariableId (n :: Nat) (t :: NCDataType a) = NCVariableId CInt deriving Eq

data SomeNCVariable where
    SomeNCVariable :: forall a (n :: Nat) (t :: NCDataType a). KnownNat n =>
        NCDataType a -> NCVariableId n t -> SomeNCVariable

type NCType   = TType   NCDataType
type NCScalar = TScalar NCDataType
type NCVector = TVector NCDataType
