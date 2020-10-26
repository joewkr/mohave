{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Data.Format.NetCDF.LowLevel.Definitions where

import           Foreign.C.Types

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

data NC c where
    NCFile :: CInt -> NC FileId
    NCGroup :: CInt -> NC GroupId

ncRawId :: NC id -> CInt
ncRawId (NCFile ncid) = ncid
ncRawId (NCGroup ncid) = ncid
