{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Format.HDF.LowLevel.HE(
    HDFError(..)
  , he_print
  , he_string
  , he_value
) where

import           Control.Exception (bracket)
import           Data.Int
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr

import           Data.Format.HDF.LowLevel.C.Definitions (HDFErrorCode, HDFError(..), toHDFErrorCode, fromHDFErrorCode)

foreign import ccall unsafe "fopen" c_fopen ::CString -> CString -> IO (Ptr CFile)
foreign import ccall unsafe "fclose" c_fclose :: Ptr CFile -> IO ()

foreign import ccall unsafe "HEprint" c_heprint :: Ptr CFile -> Int32 -> IO ()
foreign import ccall unsafe "HEstring" c_hestring :: HDFErrorCode -> IO CString
foreign import ccall unsafe "HEvalue" c_hevalue :: Int32 -> IO HDFErrorCode

he_print :: String -> IO ()
he_print fileName =
    withCString fileName $ \fileNamePtr ->
    withCString "w" $ \fileModePtr -> do
        bracket
            (c_fopen fileNamePtr fileModePtr)
            (c_fclose)
            (\hndl -> c_heprint hndl 0)

he_string :: HDFError -> IO String
he_string he = c_hestring (toHDFErrorCode he) >>= peekCString

he_value :: Int32 -> IO HDFError
he_value level = fromHDFErrorCode <$> c_hevalue level
