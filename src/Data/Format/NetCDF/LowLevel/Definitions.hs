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

-- Memory allocation mode for NetCDF strings. M -- allocated space is managed
-- by the runtime and will be allocated automatically. U -- allocation is not
-- managed and should be allocated manually by user.
data NCStringMode = M | U

newtype NCStringPtr (mode :: NCStringMode) = NCStringPtr CString deriving (Eq, Show)

instance Storable (NCStringPtr mode) where
    sizeOf    (NCStringPtr strPtr) = sizeOf strPtr
    alignment (NCStringPtr strPtr) = alignment strPtr
    peek ptr                        = NCStringPtr <$> peek (castPtr ptr)
    poke ptr  (NCStringPtr strPtr) = poke (castPtr ptr) strPtr

type family AllowedEnumTypes (a :: NCDataTypeTag) :: NCDataTypeTag where
  AllowedEnumTypes TNCUByte = TNCUByte
  AllowedEnumTypes TNCChar  = TNCChar

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

data NCUserTypeClass =
    NCVlen
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
  , ncAttributeNValues :: Word32
  , ncAttributeParentVariable :: Maybe (NCVariableId vn vt)
} -> NCAttribute t

data SomeNCAttribute where
  SomeNCAttribute :: forall (t :: NCDataTypeTag). NCDataTypeTagS t -> NCAttribute t -> SomeNCAttribute

data NCError =
    NC_NOERR
  | NC2_ERR

  | NC_EBADID
  | NC_ENFILE
  | NC_EEXIST
  | NC_EINVAL
  | NC_EPERM

  | NC_ENOTINDEFINE

  | NC_EINDEFINE

  | NC_EINVALCOORDS

  | NC_EMAXDIMS

  | NC_ENAMEINUSE
  | NC_ENOTATT
  | NC_EMAXATTS
  | NC_EBADTYPE
  | NC_EBADDIM
  | NC_EUNLIMPOS

  | NC_EMAXVARS

  | NC_ENOTVAR
  | NC_EGLOBAL
  | NC_ENOTNC
  | NC_ESTS
  | NC_EMAXNAME
  | NC_EUNLIMIT
  | NC_ENORECVARS
  | NC_ECHAR

  | NC_EEDGE
  | NC_ESTRIDE
  | NC_EBADNAME

  | NC_ERANGE
  | NC_ENOMEM
  | NC_EVARSIZE
  | NC_EDIMSIZE
  | NC_ETRUNC
  | NC_EAXISTYPE

  | NC_EDAP
  | NC_ECURL
  | NC_EIO
  | NC_ENODATA
  | NC_EDAPSVC
  | NC_EDAS
  | NC_EDDS
--  | NC_EDMR     -- #define NC_EDMR         NC_EDDS        /**< Dap4 alias */
  | NC_EDATADDS
--  | NC_EDATADAP -- #define NC_EDATADAP     NC_EDATADDS    /**< Dap4 alias */
  | NC_EDAPURL
  | NC_EDAPCONSTRAINT
  | NC_ETRANSLATION
  | NC_EACCESS
  | NC_EAUTH

  | NC_ENOTFOUND
  | NC_ECANTREMOVE
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,1)
  | NC_EINTERNAL
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,2)
  | NC_EPNETCDF
#endif

--  | NC4_FIRST_ERROR
  | NC_EHDFERR
  | NC_ECANTREAD
  | NC_ECANTWRITE
  | NC_ECANTCREATE
  | NC_EFILEMETA
  | NC_EDIMMETA
  | NC_EATTMETA
  | NC_EVARMETA
  | NC_ENOCOMPOUND
  | NC_EATTEXISTS
  | NC_ENOTNC4
  | NC_ESTRICTNC3
  | NC_ENOTNC3
  | NC_ENOPAR
  | NC_EPARINIT
  | NC_EBADGRPID
  | NC_EBADTYPID
  | NC_ETYPDEFINED
  | NC_EBADFIELD
  | NC_EBADCLASS
  | NC_EMAPTYPE
  | NC_ELATEFILL
  | NC_ELATEDEF
  | NC_EDIMSCALE
  | NC_ENOGRP
  | NC_ESTORAGE
  | NC_EBADCHUNK
  | NC_ENOTBUILT
  | NC_EDISKLESS
  | NC_ECANTEXTEND
  | NC_EMPI

  | NC_EFILTER
  | NC_ERCFILE
  | NC_ENULLPAD
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,6,2)
  | NC_EINMEMORY
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,7,4)
  | NC_ENOFILTER
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,8,0)
  | NC_ENCZARR
  | NC_ES3
  | NC_EEMPTY
#endif
#if PKG_CONFIG_NETCDF_VERSION >= PKG_VERSION(4,8,1)
  | NC_EOBJECT
  | NC_ENOOBJECT
  | NC_EPLUGIN
#endif

--  | NC4_LAST_ERROR
  | NC_UNEXPECTED
  | NC_OTHER_ERROR CInt
  deriving (Show, Eq)