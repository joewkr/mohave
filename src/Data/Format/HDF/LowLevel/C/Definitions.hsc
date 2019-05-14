{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Format.HDF.LowLevel.C.Definitions where

import           Data.Int
import           Data.Word
import           Foreign.Storable

#include <hdf.h>
#include <mfhdf.h>

newtype HDFOpenOption = HDFOpenOption { unHDFOpenOption :: Int32 }

#{enum HDFOpenOption, HDFOpenOption
  , hdf_read                 = DFACC_READ
  , hdf_write                = DFACC_WRITE
  , hdf_create               = DFACC_CREATE
  }

newtype HDFDataType = HDFDataType { unHDFDataType :: Int32 }

#{enum HDFDataType, HDFDataType
  , hdf_char8                = DFNT_CHAR8
  , hdf_uchar8               = DFNT_UCHAR8
  , hdf_int8                 = DFNT_INT8
  , hdf_uint8                = DFNT_UINT8
  , hdf_int16                = DFNT_INT16
  , hdf_uint16               = DFNT_UINT16
  , hdf_int32                = DFNT_INT32
  , hdf_uint32               = DFNT_UINT32
  , hdf_float32              = DFNT_FLOAT32
  , hdf_float64              = DFNT_FLOAT64
  }

hdfMaxVarDims :: Int32
hdfMaxVarDims = #const MAX_VAR_DIMS


data HDFVarList = HDFVarList {
    hdf_var_index :: Int32
  , hdf_var_type  :: #{type hdf_vartype_t}
} deriving (Eq, Show)

instance Storable HDFVarList where
  alignment _ = #{alignment hdf_varlist_t}
  sizeOf _ = #{size hdf_varlist_t}
  peek ptr = do
    var_index <- #{peek hdf_varlist_t, var_index} ptr
    var_type  <- #{peek hdf_varlist_t, var_type} ptr
    return $! HDFVarList var_index var_type
  poke ptr (HDFVarList var_index var_type) = do
    #{poke hdf_varlist_t, var_index} ptr var_index
    #{poke hdf_varlist_t, var_type}  ptr var_type
