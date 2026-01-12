{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Data.Format.NetCDF.LowLevel.Attribute(
    nc_inq_att
  , nc_inq_attid
  , nc_inq_attname
  , nc_inq_natts
  , nc_inq_atttype
  , nc_inq_attlen
  , nc_rename_att
  , nc_del_att
  , nc_get_att
  , nc_get_scalar_att
  , nc_put_att
  , nc_put_scalar_att
) where

import           Data.Int
import           Data.Maybe (maybe)
import qualified Data.Vector.Storable as VS
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils (with)
import           Foreign.Storable
import           GHC.TypeNats (Nat)

import           Internal.Definitions
import           Data.Format.NetCDF.LowLevel.Definitions
import           Data.Format.NetCDF.LowLevel.C.Definitions
import           Data.Format.NetCDF.LowLevel.User.Type

foreign import ccall unsafe "nc_inq_att" c_nc_inq_att :: CInt -> CInt -> CString -> Ptr CInt -> Ptr CSize -> IO CInt
foreign import ccall unsafe "nc_inq_attid" c_nc_inq_attid :: CInt -> CInt -> CString -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_attname" c_nc_inq_attname :: CInt -> CInt -> CInt -> CString -> IO CInt
foreign import ccall unsafe "nc_inq_natts" c_nc_inq_natts :: CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_atttype" c_nc_inq_atttype :: CInt -> CInt -> CString -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_attlen" c_nc_inq_attlen :: CInt -> CInt -> CString -> Ptr CSize -> IO CInt

foreign import ccall unsafe "nc_get_att" c_nc_get_att :: CInt -> CInt -> CString -> Ptr NCData -> IO CInt
-- int     nc_get_att_text (int ncid, int varid, const char *name, char *value)
-- int     nc_get_att_schar (int ncid, int varid, const char *name, signed char *value)
-- int     nc_get_att_uchar (int ncid, int varid, const char *name, unsigned char *value)
-- int     nc_get_att_short (int ncid, int varid, const char *name, short *value)
-- int     nc_get_att_int (int ncid, int varid, const char *name, int *value)
-- int     nc_get_att_long (int ncid, int varid, const char *name, long *value)
-- int     nc_get_att_float (int ncid, int varid, const char *name, float *value)
-- int     nc_get_att_double (int ncid, int varid, const char *name, double *value)
-- int     nc_get_att_ubyte (int ncid, int varid, const char *name, unsigned char *value)
-- int     nc_get_att_ushort (int ncid, int varid, const char *name, unsigned short *value)
-- int     nc_get_att_uint (int ncid, int varid, const char *name, unsigned int *value)
-- int     nc_get_att_longlong (int ncid, int varid, const char *name, long long *value)
-- int     nc_get_att_ulonglong (int ncid, int varid, const char *name, unsigned long long *value)
-- int     nc_get_att_string (int ncid, int varid, const char *name, char **value)

foreign import ccall unsafe "nc_put_att" c_nc_put_att :: CInt -> CInt -> CString -> CInt -> CSize -> Ptr NCData -> IO CInt
-- int     nc_put_att_string (int ncid, int varid, const char *name, size_t len, const char **value)
-- int     nc_put_att_text (int ncid, int varid, const char *name, size_t len, const char *value)
-- int     nc_put_att_schar (int ncid, int varid, const char *name, nc_type xtype, size_t len, const signed char *value)
-- int     nc_put_att_uchar (int ncid, int varid, const char *name, nc_type xtype, size_t len, const unsigned char *value)
-- int     nc_put_att_short (int ncid, int varid, const char *name, nc_type xtype, size_t len, const short *value)
-- int     nc_put_att_int (int ncid, int varid, const char *name, nc_type xtype, size_t len, const int *value)
-- int     nc_put_att_long (int ncid, int varid, const char *name, nc_type xtype, size_t len, const long *value)
-- int     nc_put_att_float (int ncid, int varid, const char *name, nc_type xtype, size_t len, const float *value)
-- int     nc_put_att_double (int ncid, int varid, const char *name, nc_type xtype, size_t len, const double *value)
-- int     nc_put_att_ubyte (int ncid, int varid, const char *name, nc_type xtype, size_t len, const unsigned char *value)
-- int     nc_put_att_ushort (int ncid, int varid, const char *name, nc_type xtype, size_t len, const unsigned short *value)
-- int     nc_put_att_uint (int ncid, int varid, const char *name, nc_type xtype, size_t len, const unsigned int *value)
-- int     nc_put_att_longlong (int ncid, int varid, const char *name, nc_type xtype, size_t len, const long long *value)
-- int     nc_put_att_ulonglong (int ncid, int varid, const char *name, nc_type xtype, size_t len, const unsigned long long *value)

foreign import ccall unsafe "nc_rename_att" c_nc_rename_att :: CInt -> CInt -> CString -> CString -> IO CInt
foreign import ccall unsafe "nc_del_att" c_nc_del_att :: CInt -> CInt -> CString -> IO CInt

fromMaybeVarId :: forall (t :: NCDataTypeTag) (n :: Nat).
       Maybe (NCVariableId n t)
    -> CInt
fromMaybeVarId varid = maybe ncGlobalAttribute ncRawVarId varid

nc_inq_att :: forall id (t :: NCDataTypeTag) (n :: Nat).
       NC id
    -> Maybe (NCVariableId n t)
    -> String
    -> IO (Int32, SomeNCAttribute)
nc_inq_att ncid varid attrName =
    withCString attrName $ \c_attrName ->
    alloca $ \ncTypePtr ->
    alloca $ \attrLenPtr -> do
        res <- c_nc_inq_att (ncRawId ncid) (fromMaybeVarId varid) c_attrName ncTypePtr attrLenPtr
        (SomeNCType at@NCType{ncTypeTag=t}) <- peek ncTypePtr >>= fromNCTypeTag ncid
        attrLen <- fromIntegral <$> peek attrLenPtr
        return $! (fromIntegral res, SomeNCAttribute t $ NCAttribute attrName at attrLen varid)

nc_inq_attid :: forall id (t :: NCDataTypeTag) (n :: Nat).
       NC id
    -> Maybe (NCVariableId n t)
    -> String
    -> IO (Int32, Word32)
nc_inq_attid ncid varid attrName =
    withCString attrName $ \c_attrName ->
    alloca $ \attrIdPtr -> do
        res <- c_nc_inq_attid (ncRawId ncid) (fromMaybeVarId varid) c_attrName attrIdPtr
        attrId <- peek attrIdPtr
        return $! (fromIntegral res, fromIntegral attrId)

nc_inq_attname :: forall id (t :: NCDataTypeTag) (n :: Nat).
       NC id
    -> Maybe (NCVariableId n t)
    -> Word32
    -> IO (Int32, String)
nc_inq_attname ncid varid attrId =
    allocaArray0 (fromIntegral ncMaxNameLen) $ \c_attrName -> do
        res <- c_nc_inq_attname (ncRawId ncid) (fromMaybeVarId varid) (fromIntegral attrId) c_attrName
        attrName <- peekCString c_attrName
        return $! (fromIntegral res, attrName)

nc_inq_natts :: forall id.
       NC id
    -> IO (Int32, Word32)
nc_inq_natts ncid = alloca $ \numAttrPtr -> do
    res <- c_nc_inq_natts (ncRawId ncid) numAttrPtr
    numAttr <- peek numAttrPtr
    return $! (fromIntegral res, fromIntegral numAttr)

nc_inq_atttype :: forall id (t :: NCDataTypeTag) (n :: Nat).
       NC id
    -> Maybe (NCVariableId n t)
    -> String
    -> IO (Int32, SomeNCType)
nc_inq_atttype ncid varid attrName =
    withCString attrName $ \c_attrName ->
    alloca $ \ncTypePtr -> do
        res <- c_nc_inq_atttype (ncRawId ncid) (fromMaybeVarId varid) c_attrName ncTypePtr
        ncType <- peek ncTypePtr >>= fromNCTypeTag ncid
        return $! (fromIntegral res, ncType)

nc_inq_attlen :: forall id (t :: NCDataTypeTag) (n :: Nat).
       NC id
    -> Maybe (NCVariableId n t)
    -> String
    -> IO (Int32, Word32)
nc_inq_attlen ncid varid attrName =
    withCString attrName $ \c_attrName ->
    alloca $ \attrLenPtr -> do
        res <- c_nc_inq_attlen (ncRawId ncid) (fromMaybeVarId varid) c_attrName attrLenPtr
        attrLen <- peek attrLenPtr
        return $! (fromIntegral res, fromIntegral attrLen)

nc_rename_att :: forall id (t :: NCDataTypeTag) (n :: Nat).
       NC id
    -> Maybe (NCVariableId n t)
    -> String
    -> String
    -> IO (Int32, ())
nc_rename_att ncid varid attrName attrNewName =
    withCString attrName $ \c_attrName ->
    withCString attrNewName $ \c_attrNewName -> do
        res <- c_nc_rename_att (ncRawId ncid) (fromMaybeVarId varid) c_attrName c_attrNewName
        return $! (fromIntegral res, ())

nc_del_att :: forall id (t :: NCDataTypeTag) (n :: Nat).
       NC id
    -> Maybe (NCVariableId n t)
    -> String
    -> IO (Int32, ())
nc_del_att ncid varid attrName =
    withCString attrName $ \c_attrName -> do
        res <- c_nc_del_att (ncRawId ncid) (fromMaybeVarId varid) c_attrName
        return $! (fromIntegral res, ())

nc_get_att :: forall id a (t :: NCDataTypeTag). (a ~ EquivalentHaskellType t, Storable a) =>
       NC id
    -> NCAttribute t
    -> IO (Int32, VS.Vector a)
nc_get_att ncid attr@NCAttribute{ncAttributeParentVariable=varid} =
    withCString (ncAttributeName attr) $ \c_attrName -> do
        -- (fp :: ForeignPtr (EquivalentHaskellType dt)) <- mallocForeignPtrArray $ fromIntegral attrLen
        let attrLen = fromIntegral $ ncAttributeNValues attr
        fp <- mallocForeignPtrArray attrLen
        res <- withForeignPtr fp $ \attrDataPtr -> do
            c_nc_get_att (ncRawId ncid) (fromMaybeVarId varid) c_attrName (castPtr attrDataPtr)
        return $!
            ( fromIntegral res
            , VS.unsafeFromForeignPtr0 fp (fromIntegral attrLen))

nc_get_scalar_att :: forall id a (t :: NCDataTypeTag). (a ~ EquivalentHaskellType t, Storable a) =>
       NC id
    -> NCAttribute t
    -> IO (Int32, a)
nc_get_scalar_att ncid attr@NCAttribute{ncAttributeParentVariable=varid} =
    withCString (ncAttributeName attr) $ \c_attrName ->
    allocaArray (fromIntegral $ ncAttributeNValues attr) $ \attrDataPtr -> do
        res <- c_nc_get_att (ncRawId ncid) (fromMaybeVarId varid) c_attrName (castPtr attrDataPtr)
        attrValue <- peek attrDataPtr
        return $! (fromIntegral res, attrValue)

class NCAttributeContainer t where
    withAttributePtr :: Storable a => t a -> (Ptr a -> IO b) -> IO b
    attributeLen :: Storable a => t a -> Int

instance NCAttributeContainer [] where
    withAttributePtr val f = withArray val f
    attributeLen = length

instance NCAttributeContainer VS.Vector where
    withAttributePtr val f = VS.unsafeWith val f
    attributeLen = VS.length

nc_put_att :: forall id a attr (vt :: NCDataTypeTag) (at :: NCDataTypeTag) (n :: Nat). (NCAttributeContainer attr, a ~ EquivalentHaskellType at, Storable a) =>
       NC id
    -> Maybe (NCVariableId n vt)
    -> String
    -> NCType at
    -> attr a
    -> IO (Int32, NCAttribute at)
nc_put_att ncid varid attrName attrType attrValue =
    withCString attrName $ \c_attrName ->
    withAttributePtr attrValue $ \attrValuePtr -> do
        res <- c_nc_put_att
                (ncRawId ncid)
                (fromMaybeVarId varid)
                c_attrName
                (ncRawTypeId attrType)
                (fromIntegral $ attrNVals)
                (castPtr attrValuePtr)
        return $! (fromIntegral res, NCAttribute attrName attrType (fromIntegral attrNVals) varid)
  where
    attrNVals :: Int
    attrNVals = attributeLen attrValue

nc_put_scalar_att :: forall id a (vt :: NCDataTypeTag) (at :: NCDataTypeTag) (n :: Nat). (a ~ EquivalentHaskellType at, Storable a) =>
       NC id
    -> Maybe (NCVariableId n vt)
    -> String
    -> NCType at
    -> a
    -> IO (Int32, NCAttribute at)
nc_put_scalar_att ncid varid attrName attrType attrValue =
    withCString attrName $ \c_attrName ->
    with attrValue $ \attrValuePtr -> do
        res <- c_nc_put_att
                (ncRawId ncid)
                (fromMaybeVarId varid)
                c_attrName
                (ncRawTypeId attrType)
                1
                (castPtr attrValuePtr)
        return $! (fromIntegral res, NCAttribute attrName attrType 1 varid)
