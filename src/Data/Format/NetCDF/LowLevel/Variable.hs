{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-} -- Required with ghc 8.4.3
module Data.Format.NetCDF.LowLevel.Variable where

import           Data.Int
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy(..))
import           Data.Word
import           Foreign.Ptr
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Storable
import           GHC.TypeNats (natVal, someNatVal, SomeNat(..), Nat, KnownNat)

import           Internal.Definitions
import           Data.Format.NetCDF.LowLevel.Definitions
import           Data.Format.NetCDF.LowLevel.C.Definitions

foreign import ccall unsafe "nc_def_var" c_nc_def_var :: CInt -> CString -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_def_var_fill" c_nc_def_var_fill :: CInt -> CInt -> CInt -> Ptr NCData -> IO CInt
foreign import ccall unsafe "nc_def_var_deflate" c_nc_def_var_deflate :: CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "nc_def_var_fletcher32" c_nc_def_var_fletcher32 :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "nc_def_var_chunking" c_nc_def_var_chunking :: CInt -> CInt -> CInt -> Ptr CSize -> IO CInt
foreign import ccall unsafe "nc_def_var_endian" c_nc_def_var_endian :: CInt -> CInt -> CInt -> IO CInt
-- TODO: not clear how to correctly implement this
-- int     nc_def_var_filter (int ncid, int varid, unsigned int id, size_t nparams, const unsigned int *parms)

foreign import ccall unsafe "nc_rename_var" c_nc_rename_var :: CInt -> CInt -> CString -> IO CInt

-- int     nc_free_string (size_t len, char **data)

-- int     nc_set_var_chunk_cache (int ncid, int varid, size_t size, size_t nelems, float preemption)
-- int     nc_get_var_chunk_cache (int ncid, int varid, size_t *sizep, size_t *nelemsp, float *preemptionp)

-- int     nc_get_vara (int ncid, int varid, const size_t *startp, const size_t *countp, void *ip)
-- int     nc_get_vara_text (int ncid, int varid, const size_t *startp, const size_t *countp, char *ip)
-- int     nc_get_vara_schar (int ncid, int varid, const size_t *startp, const size_t *countp, signed char *ip)
-- int     nc_get_vara_uchar (int ncid, int varid, const size_t *startp, const size_t *countp, unsigned char *ip)
-- int     nc_get_vara_short (int ncid, int varid, const size_t *startp, const size_t *countp, short *ip)
-- int     nc_get_vara_int (int ncid, int varid, const size_t *startp, const size_t *countp, int *ip)
-- int     nc_get_vara_long (int ncid, int varid, const size_t *startp, const size_t *countp, long *ip)
-- int     nc_get_vara_float (int ncid, int varid, const size_t *startp, const size_t *countp, float *ip)
-- int     nc_get_vara_double (int ncid, int varid, const size_t *startp, const size_t *countp, double *ip)
-- int     nc_get_vara_ubyte (int ncid, int varid, const size_t *startp, const size_t *countp, unsigned char *ip)
-- int     nc_get_vara_ushort (int ncid, int varid, const size_t *startp, const size_t *countp, unsigned short *ip)
-- int     nc_get_vara_uint (int ncid, int varid, const size_t *startp, const size_t *countp, unsigned int *ip)
-- int     nc_get_vara_longlong (int ncid, int varid, const size_t *startp, const size_t *countp, long long *ip)
-- int     nc_get_vara_ulonglong (int ncid, int varid, const size_t *startp, const size_t *countp, unsigned long long *ip)
-- int     nc_get_vara_string (int ncid, int varid, const size_t *startp, const size_t *countp, char **ip)
-- int     nc_get_var1 (int ncid, int varid, const size_t *indexp, void *ip)
-- int     nc_get_var1_text (int ncid, int varid, const size_t *indexp, char *ip)
-- int     nc_get_var1_schar (int ncid, int varid, const size_t *indexp, signed char *ip)
-- int     nc_get_var1_uchar (int ncid, int varid, const size_t *indexp, unsigned char *ip)
-- int     nc_get_var1_short (int ncid, int varid, const size_t *indexp, short *ip)
-- int     nc_get_var1_int (int ncid, int varid, const size_t *indexp, int *ip)
-- int     nc_get_var1_long (int ncid, int varid, const size_t *indexp, long *ip)
-- int     nc_get_var1_float (int ncid, int varid, const size_t *indexp, float *ip)
-- int     nc_get_var1_double (int ncid, int varid, const size_t *indexp, double *ip)
-- int     nc_get_var1_ubyte (int ncid, int varid, const size_t *indexp, unsigned char *ip)
-- int     nc_get_var1_ushort (int ncid, int varid, const size_t *indexp, unsigned short *ip)
-- int     nc_get_var1_uint (int ncid, int varid, const size_t *indexp, unsigned int *ip)
-- int     nc_get_var1_longlong (int ncid, int varid, const size_t *indexp, long long *ip)
-- int     nc_get_var1_ulonglong (int ncid, int varid, const size_t *indexp, unsigned long long *ip)
-- int     nc_get_var1_string (int ncid, int varid, const size_t *indexp, char **ip)
-- int     nc_get_var (int ncid, int varid, void *ip)
-- int     nc_get_var_text (int ncid, int varid, char *ip)
-- int     nc_get_var_schar (int ncid, int varid, signed char *ip)
-- int     nc_get_var_uchar (int ncid, int varid, unsigned char *ip)
-- int     nc_get_var_short (int ncid, int varid, short *ip)
-- int     nc_get_var_int (int ncid, int varid, int *ip)
-- int     nc_get_var_long (int ncid, int varid, long *ip)
-- int     nc_get_var_float (int ncid, int varid, float *ip)
-- int     nc_get_var_double (int ncid, int varid, double *ip)
-- int     nc_get_var_ubyte (int ncid, int varid, unsigned char *ip)
-- int     nc_get_var_ushort (int ncid, int varid, unsigned short *ip)
-- int     nc_get_var_uint (int ncid, int varid, unsigned int *ip)
-- int     nc_get_var_longlong (int ncid, int varid, long long *ip)
-- int     nc_get_var_ulonglong (int ncid, int varid, unsigned long long *ip)
-- int     nc_get_var_string (int ncid, int varid, char **ip)
-- int     nc_get_vars (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, void *ip)
-- int     nc_get_vars_text (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, char *ip)
-- int     nc_get_vars_schar (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, signed char *ip)
-- int     nc_get_vars_uchar (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, unsigned char *ip)
-- int     nc_get_vars_short (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, short *ip)
-- int     nc_get_vars_int (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, int *ip)
-- int     nc_get_vars_long (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, long *ip)
-- int     nc_get_vars_float (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, float *ip)
-- int     nc_get_vars_double (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, double *ip)
-- int     nc_get_vars_ubyte (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, unsigned char *ip)
-- int     nc_get_vars_ushort (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, unsigned short *ip)
-- int     nc_get_vars_uint (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, unsigned int *ip)
-- int     nc_get_vars_longlong (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, long long *ip)
-- int     nc_get_vars_ulonglong (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, unsigned long long *ip)
-- int     nc_get_vars_string (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, char **ip)
-- int     nc_get_varm (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, void *ip)
-- int     nc_get_varm_schar (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, signed char *ip)
-- int     nc_get_varm_uchar (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, unsigned char *ip)
-- int     nc_get_varm_short (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, short *ip)
-- int     nc_get_varm_int (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, int *ip)
-- int     nc_get_varm_long (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, long *ip)
-- int     nc_get_varm_float (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, float *ip)
-- int     nc_get_varm_double (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, double *ip)
-- int     nc_get_varm_ubyte (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, unsigned char *ip)
-- int     nc_get_varm_ushort (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, unsigned short *ip)
-- int     nc_get_varm_uint (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, unsigned int *ip)
-- int     nc_get_varm_longlong (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, long long *ip)
-- int     nc_get_varm_ulonglong (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, unsigned long long *ip)
-- int     nc_get_varm_text (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, char *ip)
-- int     nc_get_varm_string (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, char **ip)

foreign import ccall unsafe "nc_inq_varid" c_nc_inq_varid :: CInt -> CString -> Ptr CInt -> IO CInt
-- int     nc_inq_var (int ncid, int varid, char *name, nc_type *xtypep, int *ndimsp, int *dimidsp, int *nattsp)
foreign import ccall unsafe "nc_inq_varname" c_nc_inq_varname :: CInt -> CInt -> CString -> IO CInt
foreign import ccall unsafe "nc_inq_vartype" c_nc_inq_vartype :: CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_varndims" c_nc_inq_varndims :: CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_vardimid" c_nc_inq_vardimid :: CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_varnatts" c_nc_inq_varnatts :: CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_var_deflate" c_nc_inq_var_deflate :: CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_var_fletcher32" c_nc_inq_var_fletcher32 :: CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_var_chunking" c_nc_inq_var_chunking :: CInt -> CInt -> Ptr CInt -> Ptr CSize -> IO CInt
foreign import ccall unsafe "nc_inq_var_fill" c_nc_inq_var_fill :: CInt -> CInt -> Ptr CInt -> Ptr NCData -> IO CInt
foreign import ccall unsafe "nc_inq_var_endian" c_nc_inq_var_endian :: CInt -> CInt -> Ptr CInt -> IO CInt
-- TODO: not clear how to correctly implement this
-- int     nc_inq_var_filter (int ncid, int varid, unsigned int *idp, size_t *nparamsp, unsigned int *params)
-- int     nc_inq_var_szip (int ncid, int varid, int *options_maskp, int *pixels_per_blockp)
foreign import ccall unsafe "nc_inq_unlimdims" c_nc_inq_unlimdims :: CInt -> Ptr CInt -> Ptr CInt -> IO CInt
-- int     NC_inq_var_all (int ncid, int varid, char *name, nc_type *xtypep, int *ndimsp, int *dimidsp, int *nattsp, int *shufflep, int *deflatep, int *deflate_levelp, int *fletcher32p, int *contiguousp, size_t *chunksizesp, int *no_fill, void *fill_valuep, int *endiannessp, unsigned int *idp, size_t *nparamsp, unsigned int *params)

-- int     nc_put_vara (int ncid, int varid, const size_t *startp, const size_t *countp, const void *op)
-- int     nc_put_vara_text (int ncid, int varid, const size_t *startp, const size_t *countp, const char *op)
-- int     nc_put_vara_schar (int ncid, int varid, const size_t *startp, const size_t *countp, const signed char *op)
-- int     nc_put_vara_uchar (int ncid, int varid, const size_t *startp, const size_t *countp, const unsigned char *op)
-- int     nc_put_vara_short (int ncid, int varid, const size_t *startp, const size_t *countp, const short *op)
-- int     nc_put_vara_int (int ncid, int varid, const size_t *startp, const size_t *countp, const int *op)
-- int     nc_put_vara_long (int ncid, int varid, const size_t *startp, const size_t *countp, const long *op)
-- int     nc_put_vara_float (int ncid, int varid, const size_t *startp, const size_t *countp, const float *op)
-- int     nc_put_vara_double (int ncid, int varid, const size_t *startp, const size_t *countp, const double *op)
-- int     nc_put_vara_ubyte (int ncid, int varid, const size_t *startp, const size_t *countp, const unsigned char *op)
-- int     nc_put_vara_ushort (int ncid, int varid, const size_t *startp, const size_t *countp, const unsigned short *op)
-- int     nc_put_vara_uint (int ncid, int varid, const size_t *startp, const size_t *countp, const unsigned int *op)
-- int     nc_put_vara_longlong (int ncid, int varid, const size_t *startp, const size_t *countp, const long long *op)
-- int     nc_put_vara_ulonglong (int ncid, int varid, const size_t *startp, const size_t *countp, const unsigned long long *op)
-- int     nc_put_vara_string (int ncid, int varid, const size_t *startp, const size_t *countp, const char **op)
-- int     nc_put_var1 (int ncid, int varid, const size_t *indexp, const void *op)
-- int     nc_put_var1_text (int ncid, int varid, const size_t *indexp, const char *op)
-- int     nc_put_var1_schar (int ncid, int varid, const size_t *indexp, const signed char *op)
-- int     nc_put_var1_uchar (int ncid, int varid, const size_t *indexp, const unsigned char *op)
-- int     nc_put_var1_short (int ncid, int varid, const size_t *indexp, const short *op)
-- int     nc_put_var1_int (int ncid, int varid, const size_t *indexp, const int *op)
-- int     nc_put_var1_long (int ncid, int varid, const size_t *indexp, const long *op)
-- int     nc_put_var1_float (int ncid, int varid, const size_t *indexp, const float *op)
-- int     nc_put_var1_double (int ncid, int varid, const size_t *indexp, const double *op)
-- int     nc_put_var1_ubyte (int ncid, int varid, const size_t *indexp, const unsigned char *op)
-- int     nc_put_var1_ushort (int ncid, int varid, const size_t *indexp, const unsigned short *op)
-- int     nc_put_var1_uint (int ncid, int varid, const size_t *indexp, const unsigned int *op)
-- int     nc_put_var1_longlong (int ncid, int varid, const size_t *indexp, const long long *op)
-- int     nc_put_var1_ulonglong (int ncid, int varid, const size_t *indexp, const unsigned long long *op)
-- int     nc_put_var1_string (int ncid, int varid, const size_t *indexp, const char **op)
-- int     nc_put_var (int ncid, int varid, const void *op)
-- int     nc_put_var_text (int ncid, int varid, const char *op)
-- int     nc_put_var_schar (int ncid, int varid, const signed char *op)
-- int     nc_put_var_uchar (int ncid, int varid, const unsigned char *op)
-- int     nc_put_var_short (int ncid, int varid, const short *op)
-- int     nc_put_var_int (int ncid, int varid, const int *op)
-- int     nc_put_var_long (int ncid, int varid, const long *op)
-- int     nc_put_var_float (int ncid, int varid, const float *op)
-- int     nc_put_var_double (int ncid, int varid, const double *op)
-- int     nc_put_var_ubyte (int ncid, int varid, const unsigned char *op)
-- int     nc_put_var_ushort (int ncid, int varid, const unsigned short *op)
-- int     nc_put_var_uint (int ncid, int varid, const unsigned int *op)
-- int     nc_put_var_longlong (int ncid, int varid, const long long *op)
-- int     nc_put_var_ulonglong (int ncid, int varid, const unsigned long long *op)
-- int     nc_put_var_string (int ncid, int varid, const char **op)
-- int     nc_put_vars (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const void *op)
-- int     nc_put_vars_text (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const char *op)
-- int     nc_put_vars_schar (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const signed char *op)
-- int     nc_put_vars_uchar (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const unsigned char *op)
-- int     nc_put_vars_short (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const short *op)
-- int     nc_put_vars_int (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const int *op)
-- int     nc_put_vars_long (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const long *op)
-- int     nc_put_vars_float (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const float *op)
-- int     nc_put_vars_double (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const double *op)
-- int     nc_put_vars_ubyte (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const unsigned char *op)
-- int     nc_put_vars_ushort (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const unsigned short *op)
-- int     nc_put_vars_uint (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const unsigned int *op)
-- int     nc_put_vars_longlong (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const long long *op)
-- int     nc_put_vars_ulonglong (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const unsigned long long *op)
-- int     nc_put_vars_string (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const char **op)
-- int     nc_put_varm (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, const void *op)
-- int     nc_put_varm_text (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, const char *op)
-- int     nc_put_varm_schar (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, const signed char *op)
-- int     nc_put_varm_uchar (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, const unsigned char *op)
-- int     nc_put_varm_short (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, const short *op)
-- int     nc_put_varm_int (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, const int *op)
-- int     nc_put_varm_long (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, const long *op)
-- int     nc_put_varm_float (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, const float *op)
-- int     nc_put_varm_double (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, const double *op)
-- int     nc_put_varm_ubyte (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, const unsigned char *op)
-- int     nc_put_varm_ushort (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, const unsigned short *op)
-- int     nc_put_varm_uint (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, const unsigned int *op)
-- int     nc_put_varm_longlong (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, const long long *op)
-- int     nc_put_varm_ulonglong (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, const unsigned long long *op)
-- int     nc_put_varm_string (int ncid, int varid, const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, const ptrdiff_t *imapp, const char **op)

nc_def_var' :: forall id a (t :: NCDataType a) (n :: Nat). KnownNat n =>
       NC id
    -> String
    -> NCDataType a
    -> VarShapeDef n NCDimensionId
    -> IO (Int32, NCVariableId n t)
nc_def_var' ncid varName varType varDims =
    withCString varName $ \c_varName ->
    withStaticVectororNull varDims $ \varDimsPtr ->
    alloca $ \varidPtr -> do
        res <- c_nc_def_var (ncRawId ncid) c_varName (fromNCDataType varType) rank (castPtr varDimsPtr) varidPtr
        varid  <- peek varidPtr
        return $! (fromIntegral res, NCVariableId varid)
  where
    rank = fromIntegral $! natVal (Proxy :: Proxy n)
    withStaticVectororNull varShp f = case varShp of
        ArrayVar v -> withStaticVector v f
        ScalarVar  -> f nullPtr

nc_def_var :: forall id a (t :: NCDataType a) (n :: Nat). KnownNat n =>
       NC id
    -> String
    -> NCDataType a
    -> StaticVector n NCDimensionId
    -> IO (Int32, NCVariableId n t)
nc_def_var ncid varName varType varDims = nc_def_var' ncid varName varType (ArrayVar varDims)

nc_def_scalar_var :: forall id a (t :: NCDataType a).
       NC id
    -> String
    -> NCDataType a
    -> IO (Int32, NCVariableId 0 t)
nc_def_scalar_var ncid varName varType = nc_def_var' ncid varName varType ScalarVar

nc_def_var_fill :: forall id a (t :: NCDataType a) (n :: Nat). (KnownNat n, Storable a) =>
       NC id
    -> NCVariableId n t
    -> Maybe a
    -> IO (Int32, ())
nc_def_var_fill ncid (NCVariableId varid) fillValue =
    maybeWith with fillValue $ \fillValuePtr -> do
        res <- c_nc_def_var_fill (ncRawId ncid) varid (toNCFillTag fillMode) (castPtr fillValuePtr)
        return $! (fromIntegral res, ())
  where
    fillMode = case fillValue of
        Just{} -> NCFill
        Nothing -> NCNoFill

nc_def_var_deflate :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> NCVariableId n t
    -> Bool
    -> Maybe Int
    -> IO (Int32, ())
nc_def_var_deflate ncid (NCVariableId varid) shuffle deflateLevel = do
    res <- c_nc_def_var_deflate (ncRawId ncid) varid (fromBool shuffle) (fromBool deflate) (fromIntegral level)
    return $! (fromIntegral res, ())
  where
    (deflate,level) = case deflateLevel of
        Just l -> (True, l)
        Nothing -> (False, 0)

nc_def_var_fletcher32 :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> NCVariableId n t
    -> Bool
    -> IO (Int32, ())
nc_def_var_fletcher32 ncid (NCVariableId varid) fletcher32 = do
    res <- c_nc_def_var_fletcher32 (ncRawId ncid) varid (fromBool fletcher32)
    return $! (fromIntegral res, ())

nc_def_var_chunking :: forall id a (t :: NCDataType a) (n :: Nat). KnownNat n =>
       NC id
    -> NCVariableId n t
    -> Maybe (StaticVector n Word32)
    -> IO (Int32, ())
nc_def_var_chunking ncid (NCVariableId varid) chunkSizes =
    maybeWith withStaticVector ((fromIntegral <$>) <$> chunkSizes) $ \chunkSizesPtr -> do
        res <- c_nc_def_var_chunking (ncRawId ncid) varid (toNCStorageTypeTag storageMode) chunkSizesPtr
        return $! (fromIntegral res, ())
  where
    storageMode :: NCStorageType
    storageMode = case chunkSizes of
        Just{} -> NCChunked
        Nothing -> NCContiguous

nc_def_var_endian :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> NCVariableId n t
    -> NCEndianness
    -> IO (Int32, ())
nc_def_var_endian ncid (NCVariableId varid) endianness = do
    res <- c_nc_def_var_endian (ncRawId ncid) varid (toNCEndiannessTypeTag endianness)
    return $! (fromIntegral res, ())

nc_rename_var :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> NCVariableId n t
    -> String
    -> IO (Int32, ())
nc_rename_var ncid (NCVariableId varid) newName =
    withCString newName $ \c_newName -> do
        res <- c_nc_rename_var (ncRawId ncid) varid c_newName
        return $! (fromIntegral res, ())

nc_inq_varid :: forall id.
       NC id
    -> String
    -> IO (Int32, SomeNCVariable)
nc_inq_varid ncid varName =
    withCString varName $ \c_varName ->
    alloca $ \varidPtr -> do
        res <- c_nc_inq_varid (ncRawId ncid) c_varName varidPtr
        varid <- peek varidPtr
        (_, numDims)                 <- nc_inq_varndims ncid (NCVariableId varid)
        (_, TypedValue{valueType=t}) <- nc_inq_vartype  ncid (NCVariableId varid)
        case someNatVal (fromIntegral numDims) of
            SomeNat (_ :: Proxy n) -> return $! (
                fromIntegral res, SomeNCVariable t (NCVariableId varid :: NCVariableId n t))

nc_inq_varndims :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> NCVariableId n t
    -> IO (Int32, Word32)
nc_inq_varndims ncid (NCVariableId varid) =
    alloca $ \numDimsPtr -> do
        res <- c_nc_inq_varndims (ncRawId ncid) varid numDimsPtr
        numDims <- peek numDimsPtr
        return $! (fromIntegral res, fromIntegral numDims)

nc_inq_vartype :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> NCVariableId n t
    -> IO (Int32, NCType)
nc_inq_vartype ncid (NCVariableId varid) =
    alloca $ \ncTypePtr -> do
        res <- c_nc_inq_vartype (ncRawId ncid) varid ncTypePtr
        ncType <- fromNCTypeTag <$> peek ncTypePtr
        return $! (fromIntegral res, ncType)

nc_inq_varname :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> NCVariableId n t
    -> IO (Int32, String)
nc_inq_varname ncid (NCVariableId varid) =
    allocaArray0 (fromIntegral ncMaxNameLen) $ \c_varName -> do
        res <- c_nc_inq_varname (ncRawId ncid) varid c_varName
        varName <- peekCString c_varName
        return $! (fromIntegral res, varName)

nc_inq_vardimid :: forall id a (t :: NCDataType a) (n :: Nat). KnownNat n =>
       NC id
    -> NCVariableId n t
    -> IO (Int32, Maybe (StaticVector n NCDimensionId))
nc_inq_vardimid ncid v@(NCVariableId varid) = do
    (res1, numDims) <- nc_inq_varndims ncid v
    if (res1 /= 0)
        then return (fromIntegral res1, Nothing)
        else allocaArray (fromIntegral numDims) $ \dimIdsPtr -> do
            res2 <- c_nc_inq_vardimid (ncRawId ncid) varid dimIdsPtr
            dimIds <- toStaticVector (Proxy :: Proxy n) . map NCDimensionId <$>
                        peekArray (fromIntegral numDims) dimIdsPtr
            return $! (fromIntegral res2, dimIds)

nc_inq_varnatts :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> NCVariableId n t
    -> IO (Int32, Word32)
nc_inq_varnatts ncid (NCVariableId varid) =
    alloca $ \numAttsPtr -> do
        res <- c_nc_inq_varnatts (ncRawId ncid) varid numAttsPtr
        numAtts <- peek numAttsPtr
        return $! (fromIntegral res, fromIntegral numAtts)

data NCDeflateParams = NCDeflateParams {
    ncShuffle :: Bool
  , ncDeflateLevel :: Maybe Int
} deriving (Show, Eq)

nc_inq_var_deflate :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> NCVariableId n t
    -> IO (Int32, NCDeflateParams)
nc_inq_var_deflate ncid (NCVariableId varid) =
    alloca $ \doShufflePtr ->
    alloca $ \deflatePtr ->
    alloca $ \deflateLevelPtr -> do
        res <- c_nc_inq_var_deflate (ncRawId ncid) varid doShufflePtr deflatePtr deflateLevelPtr
        doShuffle <- toBool <$> peek doShufflePtr
        deflate <- toBool <$> peek deflatePtr
        deflateLevel <- fromIntegral <$> peek deflateLevelPtr
        return $! (
            fromIntegral res
          , NCDeflateParams doShuffle $ if deflate then Just deflateLevel else Nothing)

nc_inq_var_fletcher32 :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> NCVariableId n t
    -> IO (Int32, Bool)
nc_inq_var_fletcher32 ncid (NCVariableId varid) =
    alloca $ \fletcher32Ptr -> do
        res <- c_nc_inq_var_fletcher32 (ncRawId ncid) varid fletcher32Ptr
        fletcher32 <- toBool <$> peek fletcher32Ptr
        return $! (fromIntegral res, fletcher32)

nc_inq_var_chunking :: forall id a (t :: NCDataType a) (n :: Nat). KnownNat n =>
       NC id
    -> NCVariableId n t
    -> IO (Int32, Maybe (StaticVector n Word32))
nc_inq_var_chunking ncid (NCVariableId varid) =
    allocaArray rank $ \chunkSizesPtr -> do
    alloca $ \storageModePtr -> do
        res <- c_nc_inq_var_chunking (ncRawId ncid) varid storageModePtr chunkSizesPtr
        storageMode <- fromNCStorageTypeTag <$> peek storageModePtr
        chunkSizes <- case storageMode of
            Just NCChunked ->
                toStaticVector (Proxy :: Proxy n) . map fromIntegral <$> peekArray rank chunkSizesPtr
            _ -> return Nothing
        return $! (fromIntegral res, chunkSizes)
  where
    rank = fromIntegral $! natVal (Proxy :: Proxy n)

nc_inq_var_fill :: forall id a (t :: NCDataType a) (n :: Nat). Storable a =>
       NC id
    -> NCVariableId n t
    -> IO (Int32, Maybe a)
nc_inq_var_fill ncid (NCVariableId varid) =
    alloca $ \noFillPtr ->
    alloca $ \fillValuePtr -> do
        res <- c_nc_inq_var_fill (ncRawId ncid) varid noFillPtr (castPtr fillValuePtr)
        noFill <- peek noFillPtr
        fillValue <- if noFill == 1
            then return Nothing
            else peek fillValuePtr >>= (return . Just)
        return $! (fromIntegral res, fillValue)

nc_inq_var_endian :: forall id a (t :: NCDataType a) (n :: Nat).
       NC id
    -> NCVariableId n t
    -> IO (Int32, NCEndianness)
nc_inq_var_endian ncid (NCVariableId varid) =
    alloca $ \endiannessPtr -> do
        res <- c_nc_inq_var_endian (ncRawId ncid) varid endiannessPtr
        endianness <- fromNCEndiannessTypeTag <$> peek endiannessPtr
        case endianness of
            Just e -> return $! (fromIntegral res, e)
            Nothing -> return $! (-1, NCEndianNative) -- TODO: return a custom error code

nc_inq_num_unlimdims :: NC id -> IO (Int32, Int)
nc_inq_num_unlimdims ncid =
    alloca $ \numDimsPtr -> do
        res <- c_nc_inq_unlimdims (ncRawId ncid) numDimsPtr nullPtr
        numDims <- peek numDimsPtr
        return $! (fromIntegral res, fromIntegral numDims)

nc_inq_unlimdims :: NC id -> IO (Int32, [NCDimensionId])
nc_inq_unlimdims ncid = do
    (res1, numDimsPtr) <- nc_inq_num_unlimdims ncid
    if res1 /= 0
        then return (res1, [])
        else do
            allocaArray numDimsPtr $ \dimIdsPtr -> do
                res <- c_nc_inq_unlimdims (ncRawId ncid) nullPtr dimIdsPtr
                dimIds <- peekArray numDimsPtr dimIdsPtr
                return $! (fromIntegral res, map NCDimensionId dimIds)

