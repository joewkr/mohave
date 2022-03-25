{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
module Data.Format.NetCDF.LowLevel.Dimension(
    nc_def_dim
  , nc_inq_dim
  , nc_inq_dimid
  , nc_inq_dimlen
  , nc_inq_dimname
  , nc_inq_ndims
  , nc_inq_unlimdim
  , nc_rename_dim
) where

import           Data.Int
import           Data.Maybe (maybe)
import           Data.Word
import           Foreign.Ptr
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable

import           Data.Format.NetCDF.LowLevel.Definitions
import           Data.Format.NetCDF.LowLevel.C.Definitions

foreign import ccall unsafe "nc_def_dim" c_nc_def_dim :: CInt -> CString -> CSize -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_dim" c_nc_inq_dim :: CInt -> CInt -> CString -> Ptr CSize -> IO CInt
foreign import ccall unsafe "nc_inq_dimid" c_nc_inq_dimid :: CInt -> CString -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_dimlen" c_nc_inq_dimlen :: CInt -> CInt -> Ptr CSize -> IO CInt
foreign import ccall unsafe "nc_inq_dimname" c_nc_inq_dimname :: CInt -> CInt -> CString -> IO CInt
foreign import ccall unsafe "nc_inq_ndims" c_nc_inq_ndims :: CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_unlimdim" c_nc_inq_unlimdim :: CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_rename_dim" c_nc_rename_dim :: CInt -> CInt -> CString -> IO CInt

nc_def_dim :: NC id -> String -> Maybe Word32 -> IO (Int32, NCDimensionId)
nc_def_dim ncid dimName dimSize =
    withCString dimName $ \c_dimName ->
    alloca $ \dimIdPtr -> do
        res <- c_nc_def_dim (ncRawId ncid) c_dimName dimSize' dimIdPtr
        dimid <- peek dimIdPtr
        return $! (fromIntegral res, NCDimensionId dimid)
  where
    dimSize' :: CSize
    dimSize' =  maybe ncUnlimitedDimension fromIntegral dimSize

nc_inq_dim :: NC id -> NCDimensionId -> IO (Int32, (String, Word32))
nc_inq_dim ncid (NCDimensionId dimid) =
    allocaArray0 (fromIntegral ncMaxNameLen) $ \c_dimName ->
    alloca $ \dimSizePtr -> do
        res <- c_nc_inq_dim (ncRawId ncid) dimid c_dimName dimSizePtr
        dimName <- peekCString c_dimName
        dimSize <- peek dimSizePtr
        return $! (fromIntegral res, (dimName, fromIntegral dimSize))

nc_inq_dimid :: NC id -> String -> IO (Int32, NCDimensionId)
nc_inq_dimid ncid dimName =
    withCString dimName $ \c_dimName ->
    alloca $ \dimIdPtr -> do
        res <- c_nc_inq_dimid (ncRawId ncid) c_dimName dimIdPtr
        dimid <- peek dimIdPtr
        return $! (fromIntegral res, NCDimensionId dimid)

nc_inq_dimlen :: NC id -> NCDimensionId -> IO (Int32, Word32)
nc_inq_dimlen ncid (NCDimensionId dimid) =
    alloca $ \dimSizePtr -> do
        res <- c_nc_inq_dimlen (ncRawId ncid) dimid dimSizePtr
        dimSize <- peek dimSizePtr
        return $! (fromIntegral res, fromIntegral dimSize)

nc_inq_dimname :: NC id -> NCDimensionId -> IO (Int32, String)
nc_inq_dimname ncid (NCDimensionId dimid) =
    allocaArray0 (fromIntegral ncMaxNameLen) $ \c_dimName -> do
        res <- c_nc_inq_dimname (ncRawId ncid) dimid c_dimName
        dimName <- peekCString c_dimName
        return $! (fromIntegral res, dimName)

nc_inq_ndims :: NC id -> IO (Int32, Int)
nc_inq_ndims ncid =
    alloca $ \numDimsPtr -> do
        res <- c_nc_inq_ndims (ncRawId ncid) numDimsPtr
        numDims <- peek numDimsPtr
        return $! (fromIntegral res, fromIntegral numDims)

nc_inq_unlimdim :: NC id -> IO (Int32, NCDimensionId)
nc_inq_unlimdim ncid =
    alloca $ \dimIdPtr -> do
        res <- c_nc_inq_unlimdim (ncRawId ncid) dimIdPtr
        dimid <- peek dimIdPtr
        return $! (fromIntegral res, NCDimensionId dimid)

nc_rename_dim :: NC id -> NCDimensionId -> String -> IO (Int32, ())
nc_rename_dim ncid (NCDimensionId dimid) newDimName =
    withCString newDimName $ \c_newDimName -> do
        res <- c_nc_rename_dim (ncRawId ncid) dimid c_newDimName
        return $! (fromIntegral res, ())
