{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Format.HDF.LowLevel(
      HDFId

    , hdf_open
    , hdf_close
    ) where

import           Foreign.C.String
import           Foreign.C.Types

import           Data.Format.HDF.LowLevel.C.Definitions

foreign import ccall unsafe "Hopen" c_hopen :: CString -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "Hclose" c_hclose :: CInt -> IO CInt

newtype HDFId = HDFId CInt

hdf_open :: String -> HDFOpenOption -> IO HDFId
hdf_open fileName mode = withCString fileName $ \c_fileName -> do
    h_file <- c_hopen c_fileName (unHDFOpenOption mode) 0
    return $! HDFId h_file

hdf_close :: HDFId -> IO (CInt, ())
hdf_close (HDFId h_file) = do
    h_result <- c_hclose h_file
    return $! (h_result, ())