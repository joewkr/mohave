{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Format.HDF.LowLevel(
      HDFId

    , hdf_open
    , hdf_close
    ) where

import           Data.Int
import           Foreign.C.String
import           Foreign.C.Types

import           Data.Format.HDF.LowLevel.C.Definitions (toHDFOpenModeTag)
import           Data.Format.HDF.LowLevel.Definitions

foreign import ccall unsafe "Hopen" c_hopen :: CString -> CInt -> Int16 -> IO Int32
foreign import ccall unsafe "Hclose" c_hclose :: Int32 -> IO CInt

newtype HDFId = HDFId Int32

hdf_open :: String -> HDFOpenMode -> IO HDFId
hdf_open fileName mode = withCString fileName $ \c_fileName -> do
    h_file <- c_hopen c_fileName (fromIntegral $ toHDFOpenModeTag mode) 0
    return $! HDFId h_file

hdf_close :: HDFId -> IO (Int32, ())
hdf_close (HDFId h_file) = do
    h_result <- c_hclose h_file
    return $! (fromIntegral h_result, ())