{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Format.NetCDF.LowLevel.Definitions where

import qualified Data.ByteString as BS
import           Data.Int
import           Data.Type.Equality (TestEquality, testEquality, (:~:)(Refl))
import           Data.Word
import           Foreign.C.String (CString)
import           Foreign.C.Types
import           Foreign.Ptr (castPtr)
import           Foreign.Storable (Storable(..))
import           GHC.TypeNats (Nat, KnownNat, CmpNat)

import           Internal.Definitions
import           Internal.Numerals.Ternary

data NCData
data NCVLenData

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
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,9,0)
  | NCNoAttReord
  | NCNoDimScaleAttach
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

data NCFillMode = NCFill | NCNoFill deriving (Eq, Show)

data NCStorageType = NCContiguous | NCChunked

data NCEndianness = NCEndianNative | NCEndianLittle | NCEndianBig deriving (Eq, Show)

data NC c where
    NCFile :: CInt -> NC FileId
    NCGroup :: CInt -> NC GroupId

ncRawId :: NC id -> CInt
ncRawId (NCFile ncid) = ncid
ncRawId (NCGroup ncid) = ncid

ifFileOrGroup :: NC id -> (NC FileId -> IO a) -> (NC GroupId -> IO a) -> IO a
ifFileOrGroup ncid@NCFile{}  f _ = f ncid
ifFileOrGroup ncid@NCGroup{} _ g = g ncid

newtype NCDimensionId = NCDimensionId CInt deriving Eq

instance Storable NCDimensionId where
    sizeOf    (NCDimensionId dimId) = sizeOf dimId
    alignment (NCDimensionId dimId) = alignment dimId
    peek ptr                        = NCDimensionId <$> peek (castPtr ptr)
    poke ptr  (NCDimensionId dimId) = poke (castPtr ptr) dimId

-- Memory allocation mode for NetCDF strings and variable length arrays.
-- M -- allocated space is managed by the GHC runtime and will be deallocated
-- automatically. U -- allocation is not managed and should be deallocated
-- manually by user.
data NCAllocationMode = M | U

newtype NCStringPtr (mode :: NCAllocationMode) = NCStringPtr CString deriving (Eq, Show)

instance Storable (NCStringPtr mode) where
    sizeOf    (NCStringPtr strPtr) = sizeOf strPtr
    alignment (NCStringPtr strPtr) = alignment strPtr
    peek ptr                        = NCStringPtr <$> peek (castPtr ptr)
    poke ptr  (NCStringPtr strPtr) = poke (castPtr ptr) strPtr

data NCDataTypeTag where
    TNCNone     :: NCDataTypeTag
    TNCByte     :: NCDataTypeTag
    TNCUByte    :: NCDataTypeTag
    TNCChar     :: NCDataTypeTag
    TNCShort    :: NCDataTypeTag
    TNCUShort   :: NCDataTypeTag
    TNCInt      :: NCDataTypeTag
    TNCUInt     :: NCDataTypeTag
    TNCInt64    :: NCDataTypeTag
    TNCUInt64   :: NCDataTypeTag
    TNCFloat    :: NCDataTypeTag
    TNCDouble   :: NCDataTypeTag
    TNCString   :: NCDataTypeTag
    TNCEnum     :: NCDataTypeTag -> NCDataTypeTag
    TNCVLen     :: NCDataTypeTag -> NCDataTypeTag
    TNCOpaque   :: Nat -> NCDataTypeTag
    TNCCompound :: [(NCDataTypeTag, Nat)] -> NCDataTypeTag
    deriving (Show, Eq)

type family Insert (x :: (NCDataTypeTag, Nat)) (xs :: [( NCDataTypeTag, Nat)]) where
  Insert x '[] = x ': '[]
  Insert '(xt,xn) ('(yt,yn) ': ys) = Insert' (CmpNat xn yn) '(xt,xn) '(yt,yn) ys

type family Insert' b x y ys where
  Insert' 'LT  x y ys = x ': (y ': ys)
  Insert' _    x y ys = y ': Insert x ys

data NCDataTypeTagS (t :: NCDataTypeTag) where
    SNCNone      :: NCDataTypeTagS 'TNCNone
    SNCByte      :: NCDataTypeTagS 'TNCByte
    SNCUByte     :: NCDataTypeTagS 'TNCUByte
    SNCChar      :: NCDataTypeTagS 'TNCChar
    SNCShort     :: NCDataTypeTagS 'TNCShort
    SNCUShort    :: NCDataTypeTagS 'TNCUShort
    SNCInt       :: NCDataTypeTagS 'TNCInt
    SNCUInt      :: NCDataTypeTagS 'TNCUInt
    SNCInt64     :: NCDataTypeTagS 'TNCInt64
    SNCUInt64    :: NCDataTypeTagS 'TNCUInt64
    SNCFloat     :: NCDataTypeTagS 'TNCFloat
    SNCDouble    :: NCDataTypeTagS 'TNCDouble
    SNCString    :: NCDataTypeTagS 'TNCString
    SNCEnum      :: NCDataTypeTagS t -> NCDataTypeTagS ('TNCEnum t)
    SNCVLen      :: NCDataTypeTagS t -> NCDataTypeTagS ('TNCVLen t)
    SNCOpaque    :: TernarySNat n -> NCDataTypeTagS ('TNCOpaque n)
    SNCCompoundE :: NCDataTypeTagS ('TNCCompound '[])
    SNCCompound  :: NCDataTypeTagS t -> TernarySNat n -> NCDataTypeTagS ('TNCCompound ts) -> NCDataTypeTagS ('TNCCompound (Insert '(t, n) ts))

deriving instance Show (NCDataTypeTagS t)

type family EquivalentHaskellType (t :: NCDataTypeTag)

type instance EquivalentHaskellType TNCNone   = ()
type instance EquivalentHaskellType TNCByte   = Int8
type instance EquivalentHaskellType TNCUByte  = Word8
type instance EquivalentHaskellType TNCChar   = Int8
type instance EquivalentHaskellType TNCShort  = Int16
type instance EquivalentHaskellType TNCUShort = Word16
type instance EquivalentHaskellType TNCInt    = Int32
type instance EquivalentHaskellType TNCUInt   = Word32
type instance EquivalentHaskellType TNCInt64  = Int64
type instance EquivalentHaskellType TNCUInt64 = Word64
type instance EquivalentHaskellType TNCFloat  = Float
type instance EquivalentHaskellType TNCDouble = Double
type instance EquivalentHaskellType TNCString = (NCStringPtr 'U)  -- When reading from a NetCDF file memory is allocated on the C-side

type instance EquivalentHaskellType (TNCEnum a) = EquivalentHaskellType a

data NCUserTypeClass =
    NCVLen
  | NCOpaque
  | NCEnum
  | NCCompound deriving (Eq, Show)

data NCType (t :: NCDataTypeTag) where
  NCType :: {ncRawTypeId :: CInt, ncTypeTag :: NCDataTypeTagS t} -> NCType t

data SomeNCType where
  SomeNCType :: forall (t :: NCDataTypeTag). {ncType :: NCType t} -> SomeNCType

newtype NCVariableId (n :: Nat) (t :: NCDataTypeTag) = NCVariableId{ncRawVarId :: CInt} deriving Eq

data SomeNCVariable where
  SomeNCVariable :: forall (n :: Nat) (t :: NCDataTypeTag). KnownNat n =>
    NCDataTypeTagS t -> NCVariableId n t -> SomeNCVariable

data NCAttribute (t :: NCDataTypeTag) where
  NCAttribute :: forall t vn vt. {
    ncAttributeName :: String
  , ncAttributeType :: NCType t
  , ncAttributeNValues :: Word32
  , ncAttributeParentVariable :: Maybe (NCVariableId vn vt)
} -> NCAttribute t

data SomeNCAttribute where
  SomeNCAttribute :: forall (t :: NCDataTypeTag). NCDataTypeTagS t -> NCAttribute t -> SomeNCAttribute

