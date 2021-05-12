{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Format.NetCDF.LowLevel.Attribute where

import           Data.Int
import           Data.Maybe (fromMaybe)
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

fromMaybeVarId :: forall a (t :: NCDataType a) (n :: Nat).
       Maybe (NCVariableId n t)
    -> CInt
fromMaybeVarId varid = fromMaybe ncGlobalAttribute $ ncRawVarId <$> varid

data NCAttributeInfoRaw = NCAttributeInfoRaw {
      ncAttributeName     :: String
    , ncAttributeNValues  :: Word32
    , ncAttributeDataType :: NCType
} deriving (Show, Eq)

nc_inq_att :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> Maybe (NCVariableId n t)
    -> String
    -> IO (Int32, NCAttributeInfoRaw)
nc_inq_att ncid varid attrName =
    withCString attrName $ \c_attrName ->
    alloca $ \ncTypePtr ->
    alloca $ \attrLenPtr -> do
        res <- c_nc_inq_att (ncRawId ncid) (fromMaybeVarId varid) c_attrName ncTypePtr attrLenPtr
        ncType <- fromNCTypeTag <$> peek ncTypePtr
        attrLen <- fromIntegral <$> peek attrLenPtr
        return $! (fromIntegral res, NCAttributeInfoRaw attrName attrLen ncType)

nc_inq_attid :: forall id a (t :: NCDataType a) (n :: Nat).
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

nc_inq_attname :: forall id a (t :: NCDataType a) (n :: Nat).
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

nc_inq_atttype :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> Maybe (NCVariableId n t)
    -> String
    -> IO (Int32, NCType)
nc_inq_atttype ncid varid attrName =
    withCString attrName $ \c_attrName ->
    alloca $ \ncTypePtr -> do
        res <- c_nc_inq_atttype (ncRawId ncid) (fromMaybeVarId varid) c_attrName ncTypePtr
        ncType <- fromNCTypeTag <$> peek ncTypePtr
        return $! (fromIntegral res, ncType)

nc_inq_attlen :: forall id a (t :: NCDataType a) (n :: Nat).
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

nc_rename_att :: forall id a (t :: NCDataType a) (n :: Nat).
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

nc_del_att :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> Maybe (NCVariableId n t)
    -> String
    -> IO (Int32, ())
nc_del_att ncid varid attrName =
    withCString attrName $ \c_attrName -> do
        res <- c_nc_del_att (ncRawId ncid) (fromMaybeVarId varid) c_attrName
        return $! (fromIntegral res, ())

nc_get_att :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> Maybe (NCVariableId n t)
    -> String
    -> IO (Int32, NCVector)
nc_get_att ncid varid attrName = do
    (res1, NCAttributeInfoRaw{
          ncAttributeNValues=attrLen
        , ncAttributeDataType=TypedValue{valueType=t :: NCDataType dt}}) <- nc_inq_att ncid varid attrName
    if res1 /= 0
        then return $! (res1, TypedValue NCNone VS.empty)
        else case t of
            NCNone -> return $! (res1, TypedValue NCNone VS.empty)
            _ -> withCString attrName $ \c_attrName -> do
                (fp :: ForeignPtr dt) <- mallocForeignPtrArray $ fromIntegral attrLen
                res <- withForeignPtr fp $ \attrDataPtr -> do
                    c_nc_get_att (ncRawId ncid) (fromMaybeVarId varid) c_attrName (castPtr attrDataPtr)
                return $!
                    ( fromIntegral res
                    , TypedValue t $ VS.unsafeFromForeignPtr0 fp (fromIntegral attrLen))

class NCAttributeContainer t where
    withAttributePtr :: Storable a => t a -> (Ptr a -> IO b) -> IO b
    attributeLen :: Storable a => t a -> Int

instance NCAttributeContainer [] where
    withAttributePtr val f = withArray val f
    attributeLen = length

instance NCAttributeContainer VS.Vector where
    withAttributePtr val f = VS.unsafeWith val f
    attributeLen = VS.length

nc_put_att :: forall id a b attr (t :: NCDataType a) (n :: Nat). (NCAttributeContainer attr, Storable b) =>
       NC id
    -> Maybe (NCVariableId n t)
    -> String
    -> NCDataType b
    -> attr b
    -> IO (Int32, ())
nc_put_att ncid varid attrName attrType attrValue =
    withCString attrName $ \c_attrName ->
    withAttributePtr attrValue $ \attrValuePtr -> do
        res <- c_nc_put_att
                (ncRawId ncid)
                (fromMaybeVarId varid)
                c_attrName
                (fromNCDataType attrType)
                (fromIntegral $ attributeLen attrValue)
                (castPtr attrValuePtr)
        return $! (fromIntegral res, ())
