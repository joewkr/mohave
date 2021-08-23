{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeInType #-} -- Required with ghc 8.4.3
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
    NCString :: NCDataType (NCStringPtr 'U) -- When reading from a NetCDF file memory is allocated on the C-side

data NCDataTypeS (t :: NCDataType a) where
    SNCNone   :: NCDataTypeS 'NCNone
    SNCByte   :: NCDataTypeS 'NCByte
    SNCUByte  :: NCDataTypeS 'NCUByte
    SNCChar   :: NCDataTypeS 'NCChar
    SNCShort  :: NCDataTypeS 'NCShort
    SNCUShort :: NCDataTypeS 'NCUShort
    SNCInt    :: NCDataTypeS 'NCInt
    SNCUInt   :: NCDataTypeS 'NCUInt
    SNCInt64  :: NCDataTypeS 'NCInt64
    SNCUInt64 :: NCDataTypeS 'NCUInt64
    SNCFloat  :: NCDataTypeS 'NCFloat
    SNCDouble :: NCDataTypeS 'NCDouble
    SNCString :: NCDataTypeS 'NCString

deriving instance Show (NCDataType  a)
deriving instance Show (NCDataTypeS t)

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

newtype NCVariableId (n :: Nat) (t :: NCDataType a) = NCVariableId{ncRawVarId :: CInt} deriving Eq

data SomeNCVariable where
    SomeNCVariable :: forall a (n :: Nat) (t :: NCDataType a). KnownNat n =>
        NCDataTypeS t -> NCVariableId n t -> SomeNCVariable

type NCType   = TType   NCDataType
type NCScalar = TScalar NCDataType
type NCVector = TVector NCDataType

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