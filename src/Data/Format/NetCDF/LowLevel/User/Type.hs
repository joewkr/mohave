{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Data.Format.NetCDF.LowLevel.User.Type where

import           Data.Int
import           Data.Foldable (foldrM)
import qualified Data.Vector.Storable as VS
import           Data.Word
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils (with, copyBytes)
import           Foreign.Ptr
import           Foreign.ForeignPtr (mallocForeignPtrArray, newForeignPtr_, withForeignPtr)
import           Foreign.Storable
import           GHC.TypeNats (Nat)

import           Internal.Definitions
import           Internal.Numerals.Ternary
import           Data.Format.NetCDF.LowLevel.Definitions
import           Data.Format.NetCDF.LowLevel.C.Definitions

foreign import ccall unsafe "nc_inq_type_equal" c_nc_inq_type_equal :: CInt -> CInt -> CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_typeid" c_nc_inq_typeid :: CInt -> CString -> Ptr CInt -> IO CInt
-- int     nc_inq_typeid (int ncid, const char *name, nc_type *typeidp)
foreign import ccall unsafe "nc_inq_user_type" c_nc_inq_user_type :: CInt -> CInt -> CString -> Ptr CSize -> Ptr CInt -> Ptr CSize -> Ptr CInt -> IO CInt

foreign import ccall unsafe "nc_def_compound" c_nc_def_compound :: CInt -> CSize -> CString -> Ptr CInt -> IO CInt
-- int     nc_def_compound (int ncid, size_t size, const char *name, nc_type *typeidp)
foreign import ccall unsafe "nc_insert_compound" c_nc_insert_compound :: CInt -> CInt -> CString -> CSize -> CInt -> IO CInt
-- int     nc_insert_compound (int ncid, nc_type xtype, const char *name, size_t offset, nc_type field_typeid)
-- int     nc_insert_array_compound (int ncid, nc_type xtype, const char *name, size_t offset, nc_type field_typeid, int ndims, const int *dim_sizes)
foreign import ccall unsafe "nc_inq_compound" c_nc_inq_compound :: CInt -> CInt -> CString -> Ptr CSize -> Ptr CSize -> IO CInt
-- int     nc_inq_compound (int ncid, nc_type xtype, char *name, size_t *sizep, size_t *nfieldsp)
foreign import ccall unsafe "nc_inq_compound_name" c_nc_inq_compound_name :: CInt -> CInt -> CString -> IO CInt
-- int     nc_inq_compound_name (int ncid, nc_type xtype, char *name)
foreign import ccall unsafe "nc_inq_compound_size" c_nc_inq_compound_size :: CInt -> CInt -> Ptr CSize -> IO CInt
-- int     nc_inq_compound_size (int ncid, nc_type xtype, size_t *sizep)
foreign import ccall unsafe "nc_inq_compound_nfields" c_nc_inq_compound_nfields :: CInt -> CInt -> Ptr CSize -> IO CInt
-- int     nc_inq_compound_nfields (int ncid, nc_type xtype, size_t *nfieldsp)
foreign import ccall unsafe "nc_inq_compound_field" c_nc_inq_compound_field :: CInt -> CInt -> CInt -> CString -> Ptr CSize -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_compound_fieldname" c_nc_inq_compound_fieldname :: CInt -> CInt -> CInt -> CString -> IO CInt
-- int     nc_inq_compound_fieldname (int ncid, nc_type xtype, int fieldid, char *name)
foreign import ccall unsafe "nc_inq_compound_fieldoffset" c_nc_inq_compound_fieldoffset :: CInt -> CInt -> CInt -> Ptr CSize -> IO CInt
-- int     nc_inq_compound_fieldoffset (int ncid, nc_type xtype, int fieldid, size_t *offsetp)
foreign import ccall unsafe "nc_inq_compound_fieldtype" c_nc_inq_compound_fieldtype :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt
-- int     nc_inq_compound_fieldtype (int ncid, nc_type xtype, int fieldid, nc_type *field_typeidp)
foreign import ccall unsafe "nc_inq_compound_fieldndims" c_nc_inq_compound_fieldndims :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt
-- int     nc_inq_compound_fieldndims (int ncid, nc_type xtype, int fieldid, int *ndimsp)
foreign import ccall unsafe "nc_inq_compound_fielddim_sizes" c_nc_inq_compound_fielddim_sizes :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt
-- int     nc_inq_compound_fielddim_sizes (int ncid, nc_type xtype, int fieldid, int *dim_sizesp)
foreign import ccall unsafe "nc_inq_compound_fieldindex" c_nc_inq_compound_fieldindex :: CInt -> CInt -> CString -> Ptr CInt -> IO CInt
-- int     nc_inq_compound_fieldindex (int ncid, nc_type xtype, const char *name, int *fieldidp)

foreign import ccall unsafe "nc_def_enum" c_nc_def_enum :: CInt -> CInt -> CString -> Ptr CInt -> IO CInt
-- int     nc_def_enum (int ncid, nc_type base_typeid, const char *name, nc_type *typeidp)
foreign import ccall unsafe "nc_insert_enum" c_nc_insert_enum :: CInt -> CInt -> CString -> Ptr NCData -> IO CInt
-- int     nc_insert_enum (int ncid, nc_type xtype, const char *name, const void *value)
foreign import ccall unsafe "nc_inq_enum" c_nc_inq_enum :: CInt -> CInt -> CString -> Ptr CInt -> Ptr CSize -> Ptr CSize -> IO CInt
-- int     nc_inq_enum (int ncid, nc_type xtype, char *name, nc_type *base_nc_typep, size_t *base_sizep, size_t *num_membersp)
foreign import ccall unsafe "nc_inq_enum_member" c_nc_inq_enum_member :: CInt -> CInt -> CInt -> CString -> Ptr NCData -> IO CInt
-- int     nc_inq_enum_member (int ncid, nc_type xtype, int idx, char *name, void *value)
foreign import ccall unsafe "nc_inq_enum_ident" c_nc_inq_enum_ident :: CInt -> CInt -> CLLong -> CString -> IO CInt
-- int     nc_inq_enum_ident (int ncid, nc_type xtype, long long value, char *identifier)

foreign import ccall unsafe "nc_free_vlens" c_nc_free_vlens :: CSize -> Ptr NCVLenData -> IO CInt
-- int     nc_free_vlens (size_t nelems, nc_vlen_t vlens[])
foreign import ccall unsafe "nc_free_vlen" c_nc_free_vlen :: Ptr NCVLenData -> IO CInt
-- int     nc_free_vlen (nc_vlen_t *vl)
foreign import ccall unsafe "nc_def_vlen" c_nc_def_vlen :: CInt -> CString -> CInt -> Ptr CInt -> IO CInt
-- int     nc_def_vlen (int ncid, const char *name, nc_type base_typeid, nc_type *xtypep)
foreign import ccall unsafe "nc_inq_vlen" c_nc_inq_vlen :: CInt -> CInt -> CString -> Ptr CSize -> Ptr CInt -> IO CInt
-- int     nc_inq_vlen (int ncid, nc_type xtype, char *name, size_t *datum_sizep, nc_type *base_nc_typep)

foreign import ccall unsafe "nc_def_opaque" c_nc_def_opaque :: CInt -> CSize -> CString -> Ptr CInt -> IO CInt
-- int     nc_def_opaque (int ncid, size_t size, const char *name, nc_type *xtypep)
foreign import ccall unsafe "nc_inq_opaque" c_nc_inq_opaque :: CInt -> CInt -> CString -> Ptr CSize -> IO CInt
-- int     nc_inq_opaque (int ncid, nc_type xtype, char *name, size_t *sizep)

nc_inq_type_equal :: forall id1 id2 (t1 :: NCDataTypeTag) (t2 :: NCDataTypeTag).
     NC id1
  -> NCType t1
  -> NC id2
  -> NCType t2
  -> IO (Int32, Bool)
nc_inq_type_equal ncid1 typeid1 ncid2 typeid2 =
  alloca $ \typesEqualPtr -> do
    res <- c_nc_inq_type_equal (ncRawId ncid1) (ncRawTypeId typeid1) (ncRawId ncid2) (ncRawTypeId typeid2) typesEqualPtr
    typesEqual <- (/= 0) <$> peek typesEqualPtr
    return (fromIntegral res, typesEqual)

nc_inq_typeid :: forall id.
     NC id
  -> String
  -> IO (Int32, SomeNCType)
nc_inq_typeid ncid typeName =
  alloca $ \typeIdPtr ->
  withCString typeName $ \c_typeName -> do
    res <- c_nc_inq_typeid (ncRawId ncid) c_typeName typeIdPtr
    someNCType <- peek typeIdPtr >>= fromNCTypeTag ncid
    return (fromIntegral res, someNCType)

data NcUserTypeInfo = NcUserTypeInfo {
    ncUserTypeName      :: String
  , ncUserTypeClass     :: NCUserTypeClass
  , ncUserTypeSize      :: Word64
  , ncUserTypeBaseType  :: Maybe SomeNCType
  , ncUserTypeNumFields :: Word32
} -- deriving (Show, Eq)

shouldHaveBaseType :: NCUserTypeClass -> Bool
shouldHaveBaseType NCEnum = True
shouldHaveBaseType NCVLen = True
shouldHaveBaseType _      = False

nc_inq_user_type :: forall id (t :: NCDataTypeTag).
     NC id
  -> NCType t
  -> IO (Int32, NcUserTypeInfo)
nc_inq_user_type ncid typeid =
    allocaArray0 (fromIntegral ncMaxNameLen) $ \c_typeName ->
    alloca $ \ncTypeSizePtr ->
    alloca $ \ncBaseTypePtr ->
    alloca $ \ncTypeNFieldsPtr ->
    alloca $ \ncTypeClassPtr -> do
        res <- c_nc_inq_user_type (ncRawId ncid) (ncRawTypeId typeid) c_typeName ncTypeSizePtr ncBaseTypePtr ncTypeNFieldsPtr ncTypeClassPtr
        typeName <- peekCString c_typeName
        typeSize <- fromIntegral <$> peek ncTypeSizePtr
        maybeNcTypeClass <- fromNCUserTypeClassTag <$> peek ncTypeClassPtr
        typeNFields <- fromIntegral <$> peek ncTypeNFieldsPtr
        case maybeNcTypeClass of
          Just ncTypeClass -> do
            let
              userTypeInfoBuilder baseType = NcUserTypeInfo typeName ncTypeClass typeSize baseType typeNFields
            baseType <- if shouldHaveBaseType ncTypeClass
              then Just <$> (fromNCTypeTag ncid =<< peek ncBaseTypePtr)
              else return Nothing
            return (fromIntegral res, userTypeInfoBuilder baseType)
          Nothing -> return (-1, undefined)

nc_def_compound :: forall id.
     NC id
  -> Word64
  -> String
  -> IO (Int32, NCType ('TNCCompound '[]))
nc_def_compound ncid typeSize typeName =
  withCString typeName $ \c_typeName ->
  alloca $ \typeIdPtr -> do
    res <- c_nc_def_compound (ncRawId ncid) (fromIntegral typeSize) c_typeName typeIdPtr
    typeid <- peek typeIdPtr
    return (fromIntegral res, NCType typeid SNCCompoundE)

nc_insert_compound :: forall id (t :: NCDataTypeTag) (ts :: [( NCDataTypeTag, Nat)]) (n :: Nat).
     NC id
  -> NCType ('TNCCompound ts)
  -> String
  -> TernarySNat n
  -> NCType t
  -> IO (Int32, NCType ('TNCCompound (Insert '(t, n) ts)))
nc_insert_compound ncid (NCType ncType typeTag) fieldName fieldOffset (NCType ncFieldType ncFieldTag)=
  withCString fieldName $ \c_fieldName -> do
    res <- c_nc_insert_compound (ncRawId ncid) ncType c_fieldName (fromIntegral $ fromTernarySNat fieldOffset) ncFieldType
    return (fromIntegral res, NCType ncType (SNCCompound ncFieldTag fieldOffset typeTag))

data NCCompoundTypeInfo = NCCompoundTypeInfo {
    ncCompoundTypeName             :: String
  , ncCompoundCompoundTypeNumField :: Word32
  , ncCompoundTypeSize             :: Word64
} deriving (Eq, Show)

nc_inq_compound :: forall id (ts :: [( NCDataTypeTag, Nat)]).
     NC id
  -> NCType ('TNCCompound ts)
  -> IO (Int32, NCCompoundTypeInfo)
nc_inq_compound ncid typeid =
  allocaArray0 (fromIntegral ncMaxNameLen) $ \c_typeName ->
  alloca $ \ncTypeSizePtr ->
  alloca $ \ncTypeNFieldsPtr -> do
    res <- c_nc_inq_compound (ncRawId ncid) (ncRawTypeId typeid) c_typeName ncTypeSizePtr ncTypeNFieldsPtr
    typeName <- peekCString c_typeName
    typeSize <- fromIntegral <$> peek ncTypeSizePtr
    typeNFields <- fromIntegral <$> peek ncTypeNFieldsPtr
    return (fromIntegral res, NCCompoundTypeInfo typeName typeNFields typeSize)

nc_inq_compound_name :: forall id (ts :: [( NCDataTypeTag, Nat)]).
     NC id
  -> NCType ('TNCCompound ts)
  -> IO (Int32, String)
nc_inq_compound_name ncid typeid =
  allocaArray0 (fromIntegral ncMaxNameLen) $ \c_typeName -> do
    res <- c_nc_inq_compound_name (ncRawId ncid) (ncRawTypeId typeid) c_typeName
    typeName <- peekCString c_typeName
    return (fromIntegral res, typeName)

nc_inq_compound_size :: forall id (ts :: [( NCDataTypeTag, Nat)]).
     NC id
  -> NCType ('TNCCompound ts)
  -> IO (Int32, Word64)
nc_inq_compound_size ncid typeid =
  alloca $ \ncTypeSizePtr -> do
    res <- c_nc_inq_compound_size (ncRawId ncid) (ncRawTypeId typeid) ncTypeSizePtr
    typeSize <- peek ncTypeSizePtr
    return (fromIntegral res, fromIntegral typeSize)

nc_inq_compound_nfields :: forall id (ts :: [( NCDataTypeTag, Nat)]).
     NC id
  -> NCType ('TNCCompound ts)
  -> IO (Int32, Word32)
nc_inq_compound_nfields ncid typeid =
  alloca $ \ncTypeNFieldsPtr -> do
    res <- c_nc_inq_compound_nfields (ncRawId ncid) (ncRawTypeId typeid) ncTypeNFieldsPtr
    typeNFields <- peek ncTypeNFieldsPtr
    return (fromIntegral res, fromIntegral typeNFields)

data NCCompoundFieldInfo = NCCompoundFieldInfo {
    ncCompoundFieldName   :: String
  , ncCompoundFieldType   :: SomeNCType
  , ncCompoundFieldOffset :: Word64
  , ncCompoundFieldDims   :: [Word32]
} --deriving (Eq, Show)

nc_inq_compound_field :: forall id (t :: NCDataTypeTag).
     NC id
  -> NCType t
  -> Word32
  -> IO (Int32, NCCompoundFieldInfo)
nc_inq_compound_field ncid typeid field = do
  (res, fieldNDims, f) <- allocaArray0 (fromIntegral ncMaxNameLen) $ \c_fieldName ->
    alloca $ \ncFieldOffsetPtr ->
    alloca $ \ncFieldTypePtr ->
    alloca $ \ncFieldNDimsPtr -> do
      res <- c_nc_inq_compound_field (ncRawId ncid) (ncRawTypeId typeid) (fromIntegral field) c_fieldName ncFieldOffsetPtr ncFieldTypePtr ncFieldNDimsPtr nullPtr
      fieldName <- peekCString c_fieldName
      fieldOffset <- fromIntegral <$> peek ncFieldOffsetPtr
      fieldTypeId <- peek ncFieldTypePtr >>= fromNCTypeTag ncid
      fieldNDims <- fromIntegral <$> peek ncFieldNDimsPtr

      return (fromIntegral res, fieldNDims, NCCompoundFieldInfo fieldName fieldTypeId fieldOffset)

  if fieldNDims == 0
    then return (res, f [])
    else allocaArray fieldNDims $ \ncDimSizesPtr -> do
      _ <- c_nc_inq_compound_field (ncRawId ncid) (ncRawTypeId typeid) (fromIntegral field) nullPtr nullPtr nullPtr nullPtr ncDimSizesPtr

      ncDimSizes <- map fromIntegral <$> peekArray fieldNDims ncDimSizesPtr

      return (res, f ncDimSizes)

nc_inq_compound_fieldname :: forall id (ts :: [( NCDataTypeTag, Nat)]).
     NC id
  -> NCType ('TNCCompound ts)
  -> Word32
  -> IO (Int32, String)
nc_inq_compound_fieldname ncid typeid fieldid =
  allocaArray0 (fromIntegral ncMaxNameLen) $ \c_fieldName -> do
    res <- c_nc_inq_compound_fieldname (ncRawId ncid) (ncRawTypeId typeid) (fromIntegral fieldid) c_fieldName
    fieldName <- peekCString c_fieldName
    return (fromIntegral res, fieldName)

nc_inq_compound_fieldoffset :: forall id (ts :: [( NCDataTypeTag, Nat)]).
     NC id
  -> NCType ('TNCCompound ts)
  -> Word32
  -> IO (Int32, Word64)
nc_inq_compound_fieldoffset ncid typeid fieldid =
  alloca $ \fieldOffsetPtr -> do
    res <- c_nc_inq_compound_fieldoffset (ncRawId ncid) (ncRawTypeId typeid) (fromIntegral fieldid) fieldOffsetPtr
    fieldOffset <- peek fieldOffsetPtr
    return (fromIntegral res, fromIntegral fieldOffset)

nc_inq_compound_fieldtype :: forall id (ts :: [( NCDataTypeTag, Nat)]).
     NC id
  -> NCType ('TNCCompound ts)
  -> Word32
  -> IO (Int32, SomeNCType)
nc_inq_compound_fieldtype ncid typeid fieldid =
  alloca $ \fieldTypePtr -> do
    res <- c_nc_inq_compound_fieldtype (ncRawId ncid) (ncRawTypeId typeid) (fromIntegral fieldid) fieldTypePtr
    fieldType <- peek fieldTypePtr >>= fromNCTypeTag ncid
    return (fromIntegral res, fieldType)

nc_inq_compound_fieldndims :: forall id (ts :: [( NCDataTypeTag, Nat)]).
     NC id
  -> NCType ('TNCCompound ts)
  -> Word32
  -> IO (Int32, Int)
nc_inq_compound_fieldndims ncid typeid fieldid =
  alloca $ \fieldNDimsPtr -> do
    res <- c_nc_inq_compound_fieldndims (ncRawId ncid) (ncRawTypeId typeid) (fromIntegral fieldid) fieldNDimsPtr
    fieldNDims <- peek fieldNDimsPtr
    return (fromIntegral res, fromIntegral fieldNDims)

nc_inq_compound_fielddim_sizes :: forall id (ts :: [( NCDataTypeTag, Nat)]).
     NC id
  -> NCType ('TNCCompound ts)
  -> Word32
  -> IO (Int32, [Int])
nc_inq_compound_fielddim_sizes ncid typeid fieldid = do
  (res1, fieldNDims) <- nc_inq_compound_fieldndims ncid typeid fieldid
  if res1 /= 0 || fieldNDims == 0
    then return (res1, [])
    else allocaArray fieldNDims $ \fieldDimSizesPtr -> do
      res <- c_nc_inq_compound_fielddim_sizes (ncRawId ncid) (ncRawTypeId typeid) (fromIntegral fieldid) fieldDimSizesPtr
      fieldDimSizes <- peekArray fieldNDims fieldDimSizesPtr
      return (fromIntegral res, map fromIntegral fieldDimSizes)

nc_inq_compound_fieldindex :: forall id (ts :: [( NCDataTypeTag, Nat)]).
     NC id
  -> NCType ('TNCCompound ts)
  -> String
  -> IO (Int32, Word32)
nc_inq_compound_fieldindex ncid typeid fieldName =
  withCString fieldName $ \c_fieldName ->
  alloca $ \fieldIdxPtr -> do
    res <- c_nc_inq_compound_fieldindex (ncRawId ncid) (ncRawTypeId typeid) c_fieldName fieldIdxPtr
    fieldIdx <- peek fieldIdxPtr
    return (fromIntegral res, fromIntegral fieldIdx)

nc_def_enum :: forall id (t :: NCDataTypeTag).
  (t `OneOf` '[ 'TNCByte, 'TNCUByte, 'TNCShort, 'TNCUShort, 'TNCInt, 'TNCUInt, 'TNCInt64, 'TNCUInt64 ]) =>
     NC id
  -> NCType t
  -> String
  -> IO (Int32, NCType ('TNCEnum t))
nc_def_enum ncid (NCType cBaseType baseTypeTag) typeName =
  withCString typeName $ \c_typeName ->
  alloca $ \typeIdPtr -> do
    res <- c_nc_def_enum (ncRawId ncid) cBaseType c_typeName typeIdPtr
    typeid <- peek typeIdPtr
    return (fromIntegral res, NCType typeid $ SNCEnum baseTypeTag)

nc_insert_enum :: forall id a (t :: NCDataTypeTag).
  (a ~ EquivalentHaskellType t, Storable a) =>
     NC id
  -> NCType ('TNCEnum t)
  -> String
  -> a
  -> IO (Int32, ())
nc_insert_enum ncid (NCType ncType _) memberName memberValue =
  withCString memberName $ \c_memberName ->
  with memberValue $ \memberValuePtr -> do
    res <- c_nc_insert_enum (ncRawId ncid) ncType c_memberName (castPtr memberValuePtr)
    return (fromIntegral res, ())

data NCEnumTypeInfo = NCEnumTypeInfo {
    ncEnumTypeName     :: String
  , ncEnumBaseType     :: SomeNCType
  , ncEnumBaseTypeSize :: Word64
  , ncEnumNumMembers   :: Word32
} -- deriving (Eq, Show)

nc_inq_enum :: forall id (t :: NCDataTypeTag).
     NC id
  -> NCType ('TNCEnum t)
  -> IO (Int32, NCEnumTypeInfo)
nc_inq_enum ncid (NCType ncType _) =
  allocaArray0 (fromIntegral ncMaxNameLen) $ \c_typeName ->
  alloca $ \ncBaseTypePtr ->
  alloca $ \ncBaseTypeSizePtr ->
  alloca $ \ncNumMembersPtr -> do
    res <- c_nc_inq_enum (ncRawId ncid) ncType c_typeName ncBaseTypePtr ncBaseTypeSizePtr ncNumMembersPtr
    typeName <- peekCString c_typeName
    baseType <- peek ncBaseTypePtr >>= fromNCTypeTag ncid
    baseTypeSize <- fromIntegral <$> peek ncBaseTypeSizePtr
    numMembers <- fromIntegral <$> peek ncNumMembersPtr
    return (fromIntegral res, NCEnumTypeInfo typeName baseType baseTypeSize numMembers)

nc_inq_enum_member :: forall id a (t :: NCDataTypeTag).
  (a ~ EquivalentHaskellType t, Storable a) =>
     NC id
  -> NCType ('TNCEnum t)
  -> Word32
  -> IO (Int32, (String, a))
nc_inq_enum_member ncid (NCType ncType _) idx =
  allocaArray0 (fromIntegral ncMaxNameLen) $ \c_memberName ->
  alloca $ \memberValuePtr -> do
    res <- c_nc_inq_enum_member (ncRawId ncid) ncType (fromIntegral idx) c_memberName (castPtr memberValuePtr)
    memberName <- peekCString c_memberName
    memberValue <- peek memberValuePtr
    return (fromIntegral res, (memberName, memberValue))

nc_inq_enum_ident :: forall id (t :: NCDataTypeTag).
     NC id
  -> NCType ('TNCEnum t)
  -> Int64
  -> IO (Int32, String)
nc_inq_enum_ident ncid (NCType ncType _) value =
  allocaArray0 (fromIntegral ncMaxNameLen) $ \c_memberName -> do
    res <- c_nc_inq_enum_ident (ncRawId ncid) ncType (fromIntegral value) c_memberName
    memberName <- peekCString c_memberName
    return (fromIntegral res, memberName)

nc_def_vlen :: forall id (t :: NCDataTypeTag).
     NC id
  -> NCType t
  -> String
  -> IO (Int32, NCType ('TNCVLen t))
nc_def_vlen ncid (NCType cBaseType baseTypeTag) typeName =
  withCString typeName $ \c_typeName ->
  alloca $ \typeIdPtr -> do
    res <- c_nc_def_vlen (ncRawId ncid) c_typeName cBaseType typeIdPtr
    typeid <- peek typeIdPtr
    return (fromIntegral res, NCType typeid $ SNCVLen baseTypeTag)

data NCVLenTypeInfo = NCVLenTypeInfo {
    ncVLenTypeName     :: String
  , ncVLenBaseType     :: SomeNCType
  , ncVLenBaseTypeSize :: Word64
} -- deriving (Eq, Show)

nc_inq_vlen :: forall id (t :: NCDataTypeTag).
     NC id
  -> NCType ('TNCVLen t)
  -> IO (Int32, NCVLenTypeInfo)
nc_inq_vlen ncid (NCType ncType _) =
  allocaArray0 (fromIntegral ncMaxNameLen) $ \c_typeName ->
  alloca $ \ncBaseTypePtr ->
  alloca $ \ncBaseTypeSizePtr -> do
    res <- c_nc_inq_vlen (ncRawId ncid) ncType c_typeName ncBaseTypeSizePtr ncBaseTypePtr
    typeName <- peekCString c_typeName
    baseType <- peek ncBaseTypePtr >>= fromNCTypeTag ncid
    baseTypeSize <- fromIntegral <$> peek ncBaseTypeSizePtr
    return (fromIntegral res, NCVLenTypeInfo typeName baseType baseTypeSize)

nc_free_vlens :: Word64 -> Ptr (NCVLenContainer U a) -> IO (Int32, ())
nc_free_vlens size ptr = do
  res <- c_nc_free_vlens (fromIntegral size) (castPtr ptr)
  return (fromIntegral res, ())

nc_free_vlen :: Ptr (NCVLenContainer U a) -> IO (Int32, ())
nc_free_vlen ptr = do
  res <- c_nc_free_vlen (castPtr ptr)
  return (fromIntegral res, ())

freeVLenArray :: VS.Vector (NCVLenContainer U a) -> IO (Int32, ())
freeVLenArray vlenVector = VS.unsafeWith vlenVector (nc_free_vlens n)
  where
    n :: Word64
    n = fromIntegral $ VS.length vlenVector

peekVLenArray :: forall a (mode :: NCAllocationMode). Storable a => NCVLenContainer mode a -> IO (VS.Vector a)
peekVLenArray (NCVLenContainer n ptr) = do
  fptr <- mallocForeignPtrArray n'
  withForeignPtr fptr $ \destinationPtr -> do
    copyBytes destinationPtr ptr $ n'*(sizeOf (undefined :: a))
  return $! VS.unsafeFromForeignPtr0 fptr n'
  where
    n' :: Int
    n' = fromIntegral n

-- Perform an action on a VLen element of a Storable Vector. Unlike peekVLenArray, this
-- function does not make an extra copy of VLen data, so the temporary Storable Vector
-- with VLen data would become invalid if the correcponding NCVLenContainer is finalized.
inspectVLenArray :: forall a b (mode :: NCAllocationMode). Storable a =>
  VS.Vector (NCVLenContainer mode a) -> Int -> (VS.Vector a -> IO b) -> IO b
inspectVLenArray vlenVector idx f
  | idx >= 0 && idx < VS.length vlenVector =
    withForeignPtr (fst $ VS.unsafeToForeignPtr0 vlenVector) $ \vecPtr -> do
      (NCVLenContainer n ptr) <- peekElemOff vecPtr idx
      fptr <- newForeignPtr_ ptr
      f $! VS.unsafeFromForeignPtr0 fptr (fromIntegral n)
  | otherwise = error $ "index out of bounds " ++ show (idx, VS.length vlenVector)

-- TODO: fix types, should be `NCVLenContainer M a`
withVLen :: forall a b. Storable a =>
  VS.Vector a -> (NCVLenContainer U a -> IO b) -> IO b
withVLen vector f = VS.unsafeWith vector $ \vectorPtr -> do
  let vlen = NCVLenContainer (fromIntegral $ VS.length vector) vectorPtr
  f vlen

withVLenList :: forall a b. Storable a =>
  [VS.Vector a] -> ([NCVLenContainer U a] -> IO b) -> IO b
withVLenList = go []
  where
    go _ [] f = f []
    go res (v:[]) f = withVLen v (\lastVLen -> f . reverse $ lastVLen:res)
    go res (v:vs) f = withVLen v $ \vlen -> go (vlen:res) vs f

nc_def_opaque :: forall id (n :: Nat).
     NC id
  -> TernarySNat n
  -> String
  -> IO (Int32, NCType ('TNCOpaque n))
nc_def_opaque ncid typeSize typeName  =
  withCString typeName $ \c_typeName ->
  alloca $ \typeIdPtr -> do
    res <- c_nc_def_opaque (ncRawId ncid) (fromIntegral $ fromTernarySNat typeSize) c_typeName typeIdPtr
    typeid <- peek typeIdPtr
    return (fromIntegral res, NCType typeid $ SNCOpaque typeSize)

data NCOpaqueTypeInfo = NCOpaqueTypeInfo {
    ncOpaqueTypeName :: String
  , ncOpaqueTypeSize :: Word64
} deriving (Eq, Show)

nc_inq_opaque :: forall id (n :: Nat).
     NC id
  -> NCType ('TNCOpaque n)
  -> IO (Int32, NCOpaqueTypeInfo)
nc_inq_opaque ncid typeid =
  allocaArray0 (fromIntegral ncMaxNameLen) $ \c_typeName ->
  alloca $ \ncTypeSizePtr -> do
    res <- c_nc_inq_opaque (ncRawId ncid) (ncRawTypeId typeid) c_typeName ncTypeSizePtr
    typeName <- peekCString c_typeName
    typeSize <- fromIntegral <$> peek ncTypeSizePtr
    return (fromIntegral res, NCOpaqueTypeInfo typeName typeSize)

data CompoundWrapper where
  CompoundWrapper :: forall a. (NCDataTypeTagS ('TNCCompound a)) -> CompoundWrapper

fromNCTypeTag :: forall id. NC id -> CInt -> IO SomeNCType
fromNCTypeTag ncid tag = case fromNCStandardTypeTag tag of
  Just atomicType -> return atomicType
  Nothing -> do -- Some user-defind type
    (res, typeInfo) <- nc_inq_user_type ncid dummyTypeid
    if res /= 0
      then return (SomeNCType $ NCType undefined SNCNone)
      else case ncUserTypeClass typeInfo of
        NCCompound -> do
          (CompoundWrapper tagS) <- foldrM
            (\fieldIdx typeid -> updateType typeid . snd <$> nc_inq_compound_field ncid dummyTypeid fieldIdx)
            (CompoundWrapper SNCCompoundE)
            [0..(ncUserTypeNumFields typeInfo) - 1]
          return . SomeNCType $ NCType{ncRawTypeId=tag, ncTypeTag=tagS}
        NCEnum -> do
          enumInfo <- snd <$> nc_inq_enum ncid dummyTypeid
          case ncEnumBaseType enumInfo of
            SomeNCType{ncType=NCType{ncTypeTag=t}} ->
              return . SomeNCType $ NCType{ncRawTypeId=tag, ncTypeTag=SNCEnum t}
        NCVLen -> do
          vlenInfo <- snd <$> nc_inq_vlen ncid dummyTypeid
          case ncVLenBaseType vlenInfo of
            SomeNCType{ncType=NCType{ncTypeTag=t}} ->
              return . SomeNCType $ NCType{ncRawTypeId=tag, ncTypeTag=SNCVLen t}
        NCOpaque -> do
          opaqueInfo <- snd <$> nc_inq_opaque ncid dummyTypeid
          case toTernarySNat (fromIntegral $ ncOpaqueTypeSize opaqueInfo) of
            SomeTernarySNat p -> return . SomeNCType $ NCType{ncRawTypeId=tag, ncTypeTag=SNCOpaque p}
  where
    dummyTypeid :: NCType t
    dummyTypeid = NCType tag undefined

    updateType :: CompoundWrapper -> NCCompoundFieldInfo -> CompoundWrapper
    updateType (CompoundWrapper tagS) fieldInfo@NCCompoundFieldInfo{ncCompoundFieldType=(SomeNCType NCType{ncTypeTag=fieldTag})} =
      case toTernarySNat (fromIntegral $ ncCompoundFieldOffset fieldInfo) of
        SomeTernarySNat p -> CompoundWrapper (SNCCompound fieldTag p tagS)
