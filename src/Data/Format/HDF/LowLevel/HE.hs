{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Data.Format.HDF.LowLevel.HE(
    HDFError(..)
  , he_print
  , he_string
  , he_value
  , he_push
  , he_report
  , he_clear
) where

import           Control.Exception (bracket)
import           Data.Int
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr

import           Data.Format.HDF.LowLevel.C.Definitions (HDFErrorCode, toHDFErrorCode, fromHDFErrorCode)
import           Data.Format.HDF.LowLevel.Definitions (HDFError(..))
import           Foreign.C.Lib.FFI

foreign import ccall unsafe "fopen" c_fopen ::CString -> CString -> IO (Ptr CFile)
foreign import ccall unsafe "fclose" c_fclose :: Ptr CFile -> IO ()

foreign import ccall unsafe "HEprint" c_heprint :: Ptr CFile -> Int32 -> IO ()
foreign import ccall unsafe "HEstring" c_hestring :: HDFErrorCode -> IO CString
foreign import ccall unsafe "HEvalue" c_hevalue :: Int32 -> IO HDFErrorCode
foreign import ccall unsafe "HEpush" c_hepush :: HDFErrorCode -> CString -> CString -> CInt -> IO ()
foreign import ccall unsafe "wrp_HEclear" c_heclear :: IO ()  -- HEclear is a C macro as in hdf 4.2.14

-- HEreport is a variadic function and cannot be used directly with Haskell FFI, but
-- we could call it directly via libffi interface.
foreign import ccall "&HEreport"  c_hereport_ptr  :: FunPtr ()


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

he_push :: HDFError -> String -> String -> Int -> IO ()
he_push he functionName fileName line =
  withCString functionName $ \functionNamePtr ->
  withCString fileName $ \fileNamePtr -> do
    c_hepush (toHDFErrorCode he) functionNamePtr fileNamePtr (fromIntegral line)

class HEReportType t where
  collect :: String -> [FFIArg] -> t

instance (FFIPrimitveType a, HEReportType r) => HEReportType (a -> r) where
  collect format vaArgs = \a -> collect format (FFIArg a : vaArgs)

instance (a ~ ()) => HEReportType (IO a) where
  collect format vaArgs = withCString format $ \formatPtr -> do
      ffiVariadicFunctionCall c_hereport_ptr [FFIArg formatPtr] vaArgs

he_report :: HEReportType r => String -> r
he_report format = collect format []

he_clear :: IO ()
he_clear = c_heclear
