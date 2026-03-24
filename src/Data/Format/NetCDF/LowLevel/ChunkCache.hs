{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module Data.Format.NetCDF.LowLevel.ChunkCache(
    NCChunkCacheParams(..)
  , nc_get_chunk_cache
  , nc_get_var_chunk_cache
  , nc_set_chunk_cache
  , nc_set_var_chunk_cache
) where

import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable (peek)
import           GHC.TypeNats (Nat)

import           Data.Format.NetCDF.LowLevel.Definitions

foreign import ccall safe "nc_set_chunk_cache" c_nc_set_chunk_cache :: CSize -> CSize -> CFloat -> IO CInt
foreign import ccall safe "nc_get_chunk_cache" c_nc_get_chunk_cache :: Ptr CSize -> Ptr CSize -> Ptr CFloat -> IO CInt

foreign import ccall safe "nc_set_var_chunk_cache" c_nc_set_var_chunk_cache :: CInt -> CInt -> CSize -> CSize -> CFloat -> IO CInt
foreign import ccall safe "nc_get_var_chunk_cache" c_nc_get_var_chunk_cache :: CInt -> CInt -> Ptr CSize -> Ptr CSize -> Ptr CFloat -> IO CInt

data NCChunkCacheParams = NCChunkCacheParams {
    cacheSize       :: Int
  , cacheNElems     :: Int
  , cachePreemption :: Float
} deriving (Show, Eq)

nc_set_chunk_cache :: NCChunkCacheParams -> IO (CInt, ())
nc_set_chunk_cache NCChunkCacheParams{cacheSize=s, cacheNElems=n, cachePreemption=p} = do
    res <- c_nc_set_chunk_cache (fromIntegral s) (fromIntegral n) (realToFrac p)
    return (res, ())

nc_set_var_chunk_cache :: forall id (t :: NCDataTypeTag) (n :: Nat).
       NC id
    -> NCVariableId n t
    -> NCChunkCacheParams
    -> IO (CInt, ())
nc_set_var_chunk_cache ncid (NCVariableId varid) NCChunkCacheParams{cacheSize=s, cacheNElems=n, cachePreemption=p} = do
    res <- c_nc_set_var_chunk_cache (ncRawId ncid) varid (fromIntegral s) (fromIntegral n) (realToFrac p)
    return (res, ())

nc_get_chunk_cache :: IO (CInt, NCChunkCacheParams)
nc_get_chunk_cache =
    alloca $ \sPtr ->
    alloca $ \nPtr ->
    alloca $ \pPtr -> do
        res <- c_nc_get_chunk_cache sPtr nPtr pPtr
        s <- peek sPtr
        n <- peek nPtr
        p <- peek pPtr
        let cacheParams = NCChunkCacheParams{
            cacheSize=fromIntegral s
          , cacheNElems=fromIntegral n
          , cachePreemption=realToFrac p}
        return (res, cacheParams)

nc_get_var_chunk_cache :: forall id (t :: NCDataTypeTag) (n :: Nat).
       NC id
    -> NCVariableId n t
    -> IO (CInt, NCChunkCacheParams)
nc_get_var_chunk_cache ncid (NCVariableId varid) =
    alloca $ \sPtr ->
    alloca $ \nPtr ->
    alloca $ \pPtr -> do
        res <- c_nc_get_var_chunk_cache (ncRawId ncid) varid sPtr nPtr pPtr
        s <- peek sPtr
        n <- peek nPtr
        p <- peek pPtr
        let cacheParams = NCChunkCacheParams{
            cacheSize=fromIntegral s
          , cacheNElems=fromIntegral n
          , cachePreemption=realToFrac p}
        return (res, cacheParams)

