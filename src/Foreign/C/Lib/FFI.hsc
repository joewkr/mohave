{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Foreign.C.Lib.FFI(ffiVariadicFunctionCall, FFIPrimitveType, FFIArg(..)) where

import           Data.Int
import           Data.Word
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable (Storable, peek)

#include <ffi.h>
#include <limits.h>

data FFI_Type
data FFI_CIF

type FFIRetCode = #{type ffi_status}

foreign import ccall unsafe "ffi_prep_cif_var" c_ffi_prep_cif_var :: Ptr FFI_CIF -> FFIAbi -> CUInt -> CUInt -> Ptr FFI_Type -> Ptr (Ptr FFI_Type) -> IO FFIRetCode
foreign import ccall unsafe "ffi_call" c_ffi_call :: Ptr FFI_CIF -> FunPtr a -> Ptr b -> Ptr (Ptr FFIArg) -> IO FFIRetCode
foreign import ccall "&ffi_type_double"     ffi_double     :: Ptr FFI_Type
foreign import ccall "&ffi_type_float"      ffi_float      :: Ptr FFI_Type
-- No Haskell equivalent for long double C type
-- foreign import ccall "&ffi_type_longdouble" ffi_longdouble :: Ptr FFI_Type
foreign import ccall "&ffi_type_pointer"    ffi_pointer    :: Ptr FFI_Type
foreign import ccall "&ffi_type_sint16"     ffi_sint16     :: Ptr FFI_Type
foreign import ccall "&ffi_type_sint32"     ffi_sint32     :: Ptr FFI_Type
foreign import ccall "&ffi_type_sint64"     ffi_sint64     :: Ptr FFI_Type
foreign import ccall "&ffi_type_sint8"      ffi_sint8      :: Ptr FFI_Type
foreign import ccall "&ffi_type_uint16"     ffi_uint16     :: Ptr FFI_Type
foreign import ccall "&ffi_type_uint32"     ffi_uint32     :: Ptr FFI_Type
foreign import ccall "&ffi_type_uint64"     ffi_uint64     :: Ptr FFI_Type
foreign import ccall "&ffi_type_uint8"      ffi_uint8      :: Ptr FFI_Type
foreign import ccall "&ffi_type_void"       ffi_void       :: Ptr FFI_Type

#if SCHAR_MAX == 127
foreign import ccall "&ffi_type_uint8"       ffi_uchar     :: Ptr FFI_Type
foreign import ccall "&ffi_type_sint8"       ffi_schar     :: Ptr FFI_Type
#else
  #error "char size not supported"
#endif

#if SHRT_MAX == 32767
foreign import ccall "&ffi_type_uint16"      ffi_ushort    :: Ptr FFI_Type
foreign import ccall "&ffi_type_sint16"      ffi_sshort    :: Ptr FFI_Type
#elif SHRT_MAX == 2147483647
foreign import ccall "&ffi_type_uint32"      ffi_ushort    :: Ptr FFI_Type
foreign import ccall "&ffi_type_sint32"      ffi_sshort    :: Ptr FFI_Type
#else
  #error "short size not supported"
#endif

#if INT_MAX == 32767
foreign import ccall "&ffi_type_uint16"      ffi_uint      :: Ptr FFI_Type
foreign import ccall "&ffi_type_sint16"      ffi_sint      :: Ptr FFI_Type
#elif INT_MAX == 2147483647
foreign import ccall "&ffi_type_uint32"      ffi_uint      :: Ptr FFI_Type
foreign import ccall "&ffi_type_sint32"      ffi_sint      :: Ptr FFI_Type
#elif INT_MAX == 9223372036854775807
foreign import ccall "&ffi_type_uint64"      ffi_uint      :: Ptr FFI_Type
foreign import ccall "&ffi_type_sint64"      ffi_sint      :: Ptr FFI_Type
#else
 #error "int size not supported"
#endif

#if LONG_MAX == 2147483647
# if FFI_LONG_LONG_MAX != FFI_64_BIT_MAX
 #error "no 64-bit data type supported"
# endif
#elif LONG_MAX != FFI_64_BIT_MAX
 #error "long size not supported"
#endif

#if LONG_MAX == 2147483647
foreign import ccall "&ffi_type_uint32"      ffi_ulong     :: Ptr FFI_Type
foreign import ccall "&ffi_type_sint32"      ffi_slong     :: Ptr FFI_Type
#elif LONG_MAX == FFI_64_BIT_MAX
foreign import ccall "&ffi_type_uint64"      ffi_ulong     :: Ptr FFI_Type
foreign import ccall "&ffi_type_sint64"      ffi_slong     :: Ptr FFI_Type
#else
 #error "long size not supported"
#endif

class Storable a => FFIPrimitveType a where
  ffiType :: a -> Ptr FFI_Type
  ffiAlloca :: (Ptr a -> IO b) -> IO b
  ffiAlloca = alloca

instance FFIPrimitveType (Ptr a) where
  ffiType _ = ffi_pointer

instance FFIPrimitveType () where
  ffiType _ = ffi_void
  ffiAlloca func = func nullPtr

instance FFIPrimitveType CFloat where
  ffiType _ = ffi_float

instance FFIPrimitveType CDouble where
  ffiType _ = ffi_double

instance FFIPrimitveType CInt where
  ffiType _ = ffi_sint

instance FFIPrimitveType Int8 where
  ffiType _ = ffi_sint8

instance FFIPrimitveType Int16 where
  ffiType _ = ffi_sint16

instance FFIPrimitveType Int32 where
  ffiType _ = ffi_sint32

instance FFIPrimitveType Int64 where
  ffiType _ = ffi_sint64

instance FFIPrimitveType CUInt where
  ffiType _ = ffi_uint

instance FFIPrimitveType Word8 where
  ffiType _ = ffi_uint8

instance FFIPrimitveType Word16 where
  ffiType _ = ffi_uint16

instance FFIPrimitveType Word32 where
  ffiType _ = ffi_uint32

instance FFIPrimitveType Word64 where
  ffiType _ = ffi_uint64

instance FFIPrimitveType CShort where
  ffiType _ = ffi_sshort

instance FFIPrimitveType CUShort where
  ffiType _ = ffi_ushort

instance FFIPrimitveType CLong where
  ffiType _ = ffi_slong

instance FFIPrimitveType CULong where
  ffiType _ = ffi_ulong

instance FFIPrimitveType CSChar where
  ffiType _ = ffi_schar

instance FFIPrimitveType CUChar where
  ffiType _ = ffi_uchar

instance FFIPrimitveType CChar where
#if (CHAR_MIN != 0)
  ffiType _ = ffi_schar
#else
  ffiType _ = ffi_uchar
#endif


data FFIArg where
  FFIArg :: FFIPrimitveType a => {unFFIArg :: a} -> FFIArg

withFFIArg :: FFIArg -> (Ptr FFIArg -> IO b) -> IO b
withFFIArg FFIArg{unFFIArg=arg} func = with arg (\ptr -> func $ castPtr ptr)

newtype FFIAbi = FFIAbi #{type ffi_abi}

ffiDefaultAbi :: FFIAbi
ffiDefaultAbi = FFIAbi #{const FFI_DEFAULT_ABI}

cifSize :: Int
cifSize = #{size ffi_cif}

ffiVariadicFunctionCall :: forall a.FFIPrimitveType a => FunPtr a -> [FFIArg] -> [FFIArg] -> IO a
ffiVariadicFunctionCall funPtr fixArgs vaArgs =
  allocaBytesAligned cifSize #{alignment ffi_cif} $ \cifPtr ->
  ffiAlloca $ \resPtr ->
  withArray (argumentTypes allArgs) $ \argTypesPtr -> do
    prep_res <- c_ffi_prep_cif_var
      cifPtr ffiDefaultAbi
      (fromIntegral $ length fixArgs)
      (fromIntegral $ length allArgs)
      (ffiType (undefined :: a))
      argTypesPtr
    if prep_res == #{const FFI_OK}
      then do
        call_res <- withArgs allArgs $ \argsPtr -> c_ffi_call cifPtr funPtr resPtr argsPtr
        if call_res == #{const FFI_OK}
          then peek resPtr
          else error "Error while preforming FFI call"
      else
        error "Error while preparing FFI CIF"
  where
    allArgs :: [FFIArg]
    allArgs = fixArgs ++ vaArgs


argumentTypes :: [FFIArg] -> [Ptr FFI_Type]
argumentTypes allArgs =
  map (\FFIArg{unFFIArg=arg} -> ffiType arg) allArgs

withArgs :: [FFIArg] -> (Ptr (Ptr FFIArg) -> IO a) -> IO a
withArgs allArgs func = go [] allArgs
  where
    go argsPtrs [] = withArray (reverse argsPtrs) func
    go argsPtrs (arg:restArgs) = withFFIArg arg (\argPtr -> go (argPtr:argsPtrs) restArgs)
