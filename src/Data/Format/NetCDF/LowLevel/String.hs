{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-} -- Required with ghc 8.4.3
module Data.Format.NetCDF.LowLevel.String where

import           Control.Monad (void)
import qualified Data.ByteString as BS
import           Data.Int
import qualified Data.Vector.Storable as VS
import           Foreign.Concurrent (addForeignPtrFinalizer)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr hiding (addForeignPtrFinalizer)
import           Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import           Foreign.Marshal.Utils (with)
import           Foreign.Ptr
import           GHC.TypeNats (Nat, KnownNat)

import           Internal.Definitions
import           Data.Format.NetCDF.LowLevel.Definitions
import           Data.Format.NetCDF.LowLevel.Variable
                  ( nc_get_vara_fptr
                  , nc_get_var_fptr
                  , nc_get_vars_fptr
                  , nc_get_var1
                  , nc_get_scalar
                  , nc_put_scalar
                  , nc_put_var1)

foreign import ccall unsafe "nc_free_string" c_nc_free_string :: CSize -> Ptr CString -> IO CInt

nc_free_string :: NCStringPtr 'U -> IO (Int32, ())
nc_free_string (NCStringPtr ncString) =
    with ncString $ \ncStringPtr -> do
        res <- c_nc_free_string 1 ncStringPtr
        return $! (fromIntegral res, ())

nc_free_string' :: Int -> Ptr CString -> IO ()
nc_free_string' size ncStringPtr = void $ c_nc_free_string (fromIntegral size) ncStringPtr

fromNCString :: NCStringPtr mode -> IO BS.ByteString
fromNCString (NCStringPtr ncString) = BS.packCString ncString

toNCString :: CString -> NCStringPtr 'U
toNCString = NCStringPtr

ncStringForeignPtrToVS :: Int32 -> (ForeignPtr (NCStringPtr 'U), Int) -> IO (Int32, VS.Vector (NCStringPtr 'M))
ncStringForeignPtrToVS res (fp, len) = if res /= 0
    then return (res, VS.empty)
    else do
        addForeignPtrFinalizer fp (nc_free_string' (fromIntegral len) (castPtr $ unsafeForeignPtrToPtr fp))
        return (res, VS.unsafeFromForeignPtr0 (castForeignPtr fp) len)

nc_get_vara_string :: forall id (n :: Nat). KnownNat n =>
       NC id
    -> NCVariableId n 'NCString
    -> StaticVector n Int
    -> StaticVector n Int
    -> IO (Int32, VS.Vector (NCStringPtr 'M))
nc_get_vara_string ncid varid start count = do
    (res, fpLen) <- nc_get_vara_fptr ncid varid start count
    ncStringForeignPtrToVS res fpLen

nc_get_var1_string :: forall id (n :: Nat). KnownNat n =>
       NC id
    -> NCVariableId n 'NCString
    -> StaticVector n Int
    -> IO (Int32, BS.ByteString)
nc_get_var1_string ncid varid start = do
    (res, ncStringPtr) <- nc_get_var1 ncid varid start
    if res /= 0
        then return (res, BS.empty)
        else do
            ncString <- fromNCString ncStringPtr
            void $ nc_free_string ncStringPtr
            return (res, ncString)

nc_get_var_string :: forall id(n :: Nat). KnownNat n =>
       NC id
    -> NCVariableId n 'NCString
    -> IO (Int32, VS.Vector (NCStringPtr 'M))
nc_get_var_string ncid varid = do
    (res, fpLen) <- nc_get_var_fptr ncid varid
    ncStringForeignPtrToVS res fpLen

nc_get_vars_string :: forall id (n :: Nat). KnownNat n =>
       NC id
    -> NCVariableId n 'NCString
    -> StaticVector n Int
    -> StaticVector n Int
    -> StaticVector n Int
    -> IO (Int32, VS.Vector (NCStringPtr 'M))
nc_get_vars_string ncid varid start count stride = do
    (res, fpLen) <- nc_get_vars_fptr ncid varid start count stride
    ncStringForeignPtrToVS res fpLen

nc_get_string :: forall id.
       NC id
    -> NCVariableId 0 'NCString
    -> IO (Int32, BS.ByteString)
nc_get_string ncid varid = do
    (res, ncStringPtr) <- nc_get_scalar ncid varid
    if res /= 0
        then return (res, BS.empty)
        else do
            ncString <- fromNCString ncStringPtr
            void $ nc_free_string ncStringPtr
            return (res, ncString)

nc_put_string :: forall id.
       NC id
    -> NCVariableId 0 'NCString
    -> BS.ByteString
    -> IO (Int32, ())
nc_put_string ncid varid ncData =
    BS.useAsCString ncData $ \ncDataPtr ->
        nc_put_scalar ncid varid (toNCString ncDataPtr)

nc_put_var1_string :: forall id (n :: Nat). KnownNat n =>
       NC id
    -> NCVariableId n 'NCString
    -> StaticVector n Int
    -> BS.ByteString
    -> IO (Int32, ())
nc_put_var1_string ncid varid start ncData =
    BS.useAsCString ncData $ \ncDataPtr ->
        nc_put_var1 ncid varid start (toNCString ncDataPtr)
