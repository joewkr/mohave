{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-} -- Required with ghc 8.4.3
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Data.Format.NetCDF.LowLevel.String(
    fromNCString
  , toNCString

  , nc_free_string
  , nc_get_vara_string
  , nc_get_var1_string
  , nc_get_var_string
  , nc_get_vars_string
  , nc_get_string
  , nc_put_string
  , nc_put_var1_string
  , nc_get_string_att
  , nc_get_scalar_string_att
  , nc_put_string_att
  , nc_put_scalar_string_att
) where

import           Control.Monad (void)
import qualified Data.ByteString as BS
import           Data.Int
import           Data.Monoid (Monoid, mempty)
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
import           Data.Format.NetCDF.LowLevel.Attribute
                  ( nc_inq_attlen
                  , nc_put_scalar_att
                  , nc_get_att
                  , nc_put_att
                    )
import           Data.Format.NetCDF.LowLevel.Error
                  ( toNCErrorCode)
import           Data.Format.NetCDF.LowLevel.File
                  ( nc_inq_format)
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

nc_get_string_att' :: forall id a r (t :: NCDataType a) (n :: Nat). Monoid r =>
       NC id
    -> Maybe (NCVariableId n t)
    -> String
    -> (VS.Vector (NCStringPtr 'U) -> IO r)
    -> (BS.ByteString -> r)
    -> IO (Int32, r)
nc_get_string_att' ncid varid attrName ncStringConvert ncStringWrap = do
    (res, (TypedValue t v)) <- nc_get_att ncid varid attrName
    if res /= 0
        then return $! (res, mempty)
        else case t of
            NCString -> do
                attrValue <- ncStringConvert v
                return $! (fromIntegral res, attrValue)
            NCChar -> do
                (res1, attrLen) <- nc_inq_attlen ncid varid attrName
                ncString <- VS.unsafeWith v $ \attrDataPtr ->
                    BS.packCStringLen (castPtr attrDataPtr, fromIntegral attrLen)
                return $! (fromIntegral res1, ncStringWrap ncString)
            _ -> return $! (fromIntegral $ toNCErrorCode NC_EBADTYPE, mempty)

nc_get_string_att :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> Maybe (NCVariableId n t)
    -> String
    -> IO (Int32, [BS.ByteString])
nc_get_string_att ncid varid attrName =
    nc_get_string_att' ncid varid attrName ncStringConvert (:[])
  where
    ncStringConvert :: VS.Vector (NCStringPtr 'U) -> IO [BS.ByteString]
    ncStringConvert v = VS.foldM'
        (\l attrValue -> (:l) <$> fromNCString attrValue <* nc_free_string attrValue)
        []
        (VS.reverse v)

nc_get_scalar_string_att :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> Maybe (NCVariableId n t)
    -> String
    -> IO (Int32, BS.ByteString)
nc_get_scalar_string_att ncid varid attrName =
    nc_get_string_att' ncid varid attrName ncStringConvert id
  where
    ncStringConvert :: VS.Vector (NCStringPtr 'U) -> IO BS.ByteString
    ncStringConvert v = do
        let attrValue = v VS.! 0
        ncString <- fromNCString attrValue
        void $ nc_free_string attrValue
        return ncString

doStringOrChar :: forall id.
       NC id
    -> IO (Int32, ())
    -> IO (Int32, ())
    -> IO (Int32, ())
doStringOrChar ncid doString doChar = do
    (res, format) <- ifFileOrGroup ncid
        nc_inq_format
        (\_ -> return (0, NCNetCDF4)) -- if ncid is a group id the file is a NetCDF4 file and supports String type
    if res /= 0
        then return $! (res, ())
        else case format of
            NCNetCDF4 -> doString
            _         -> doChar

nc_put_string_att :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> Maybe (NCVariableId n t)
    -> String
    -> [BS.ByteString]
    -> IO (Int32, ())
nc_put_string_att ncid varid attrName attrValue = doStringOrChar ncid doString doChar
  where
    doString = withNCStrings attrValue $ \ncStrings ->
        nc_put_att ncid varid attrName NCString ncStrings

    doChar   = BS.useAsCStringLen (BS.concat attrValue) $ \(attrValuePtr, attrLen) -> do
        fptr <- newForeignPtr_ (castPtr attrValuePtr)
        nc_put_att ncid varid attrName NCChar (VS.unsafeFromForeignPtr0 fptr attrLen)

withNCStrings :: [BS.ByteString] -> ([NCStringPtr 'U] -> IO (Int32, ())) -> IO (Int32, ())
withNCStrings strs func = go [] (reverse strs)
  where
    go ncStrings []     = func ncStrings
    go ncStrings (s:ss) = BS.useAsCString s (\sPtr -> go (toNCString sPtr:ncStrings) ss)

nc_put_scalar_string_att :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> Maybe (NCVariableId n t)
    -> String
    -> BS.ByteString
    -> IO (Int32, ())
nc_put_scalar_string_att ncid varid attrName attrValue = doStringOrChar ncid doString doChar
  where
    doString = BS.useAsCString attrValue $ \attrValuePtr ->
        nc_put_scalar_att ncid varid attrName NCString (toNCString attrValuePtr)

    doChar   = BS.useAsCStringLen attrValue $ \(attrValuePtr, attrLen) -> do
        fptr <- newForeignPtr_ (castPtr attrValuePtr)
        nc_put_att ncid varid attrName NCChar (VS.unsafeFromForeignPtr0 fptr attrLen)
