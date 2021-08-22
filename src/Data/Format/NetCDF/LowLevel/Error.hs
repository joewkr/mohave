{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Format.NetCDF.LowLevel.Error(nc_strerror, fromNCErrorCode, toNCErrorCode) where

import           Foreign.C.String
import           Foreign.C.Types

import           Data.Format.NetCDF.LowLevel.Definitions
import           Data.Format.NetCDF.LowLevel.C.Definitions

foreign import ccall unsafe "nc_strerror" c_nc_strerror :: CInt -> IO CString

nc_strerror :: NCError -> IO String
nc_strerror ncerr = c_nc_strerror (toNCErrorCode ncerr) >>= peekCString
