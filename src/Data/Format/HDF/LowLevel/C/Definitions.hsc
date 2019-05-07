{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Format.HDF.LowLevel.C.Definitions where

import Data.Int

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