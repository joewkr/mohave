{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
module Data.Format.NetCDF.LowLevel.File(
    NCInfo(..)

  , nc_open
  , nc_close
  , nc_sync
  , nc_redef
  , nc_enddef
  , nc_create
  , nc_inq
  , nc_inq_format
  , nc_inq_format_extended
  , nc_inq_path
  , nc_set_fill
) where

import           Data.Bits as B
import           Data.Int
import           Foreign.Ptr
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable

import           Data.Format.NetCDF.LowLevel.C.Definitions
import           Data.Format.NetCDF.LowLevel.Definitions

-- int     nc__create (const char *path, int cmode, size_t initialsz, size_t *chunksizehintp, int *ncidp)
-- int     nc__enddef (int ncid, size_t h_minfree, size_t v_align, size_t v_minfree, size_t r_align)
-- int     nc__open (const char *path, int omode, size_t *chunksizehintp, int *ncidp)
-- nc_abort -- No longer necessary for user to invoke manually.
foreign import ccall unsafe "nc_close" c_nc_close :: CInt -> IO CInt
-- int     nc_close_memio (int ncid, NC_memio *memio)
foreign import ccall unsafe "nc_create" c_nc_create :: CString -> CInt -> Ptr CInt -> IO CInt
-- int     nc_create_mem (const char *path, int mode, size_t initialsize, int *ncidp)
-- int     nc_create_par -- no parallel NetCDF support in this version of Haskell bindings
-- int     nc_create_par_fortran  -- no parallel NetCDF support in this version of Haskell bindings
-- int     nc_def_user_format -- no user-defined format support in this version of Haskell bindings
foreign import ccall unsafe "nc_enddef" c_nc_enddef :: CInt -> IO CInt
foreign import ccall unsafe "nc_inq" c_nc_inq :: CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_format" c_nc_inq_format :: CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_format_extended" c_nc_inq_format_extended :: CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_path" c_nc_inq_path :: CInt -> Ptr CSize -> CString -> IO CInt
-- int     nc_inq_type (int ncid, nc_type xtype, char *name, size_t *size)
-- int     nc_inq_user_format -- no user-defined format support in this version of Haskell bindings
foreign import ccall unsafe "nc_open" c_nc_open :: CString -> CInt -> Ptr CInt -> IO CInt
-- int     nc_open_mem (const char *path, int omode, size_t size, void *memory, int *ncidp)
-- int     nc_open_memio (const char *path, int omode, NC_memio *params, int *ncidp)
-- nc_open_par -- no parallel NetCDF support in this version of Haskell bindings
-- nc_open_par_fortran -- no parallel NetCDF support in this version of Haskell bindings
foreign import ccall unsafe "nc_redef" c_nc_redef :: CInt -> IO CInt
foreign import ccall unsafe "nc_set_fill" c_nc_set_fill :: CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_sync" c_nc_sync :: CInt -> IO CInt
-- nc_var_par_access -- no parallel NetCDF support in this version of Haskell bindings

nc_open :: String -> NCOpenMode -> IO (Int32, NC FileId)
nc_open fileName mode =
    withCString fileName $ \c_fileName ->
    alloca $ \ncIDPtr -> do
        res <- c_nc_open c_fileName (toNCOpenModeTag mode) ncIDPtr
        ncid <- peek ncIDPtr
        return $! (fromIntegral res, NCFile ncid)

nc_close :: NC FileId -> IO (Int32, ())
nc_close (NCFile ncid) = do
   res <- c_nc_close ncid
   return $! (fromIntegral res, ())

nc_sync :: NC FileId -> IO (Int32, ())
nc_sync (NCFile ncid) = do
   res <- c_nc_sync ncid
   return $! (fromIntegral res, ())

nc_redef :: NC FileId -> IO (Int32, ())
nc_redef (NCFile ncid) = do
   res <- c_nc_redef ncid
   return $! (fromIntegral res, ())

nc_enddef :: NC FileId -> IO (Int32, ())
nc_enddef (NCFile ncid) = do
   res <- c_nc_enddef ncid
   return $! (fromIntegral res, ())

nc_create :: String -> NCFormat -> NCOpenMode -> IO (Int32, NC FileId)
nc_create fileName format mode =
    withCString fileName $ \c_fileName ->
    alloca $ \ncIDPtr -> do
        res <- c_nc_create c_fileName ncCreateFlags ncIDPtr
        ncid <- peek ncIDPtr
        return $! (fromIntegral res, NCFile ncid)
  where
    ncCreateFlags :: CInt
    ncCreateFlags = (toNCFormatTag format) B..|. (toNCOpenModeTag mode)

data NCInfo = NCInfo {
    ncNumDims        :: Int
  , ncNumVars        :: Int
  , ncNumGlobalAttrs :: Int
  , ncUnlimitedDimId :: Maybe Int
} deriving (Show, Eq)

nc_inq :: NC id -> IO (Int32, NCInfo)
nc_inq ncid =
    alloca $ \nDimsPtr ->
    alloca $ \nVarsPtr ->
    alloca $ \nAttsPtr ->
    alloca $ \unlimDimIdPtr -> do
        res <- c_nc_inq (ncRawId ncid) nDimsPtr nVarsPtr nAttsPtr unlimDimIdPtr
        nDims      <- fromIntegral <$> peek nDimsPtr
        nVars      <- fromIntegral <$> peek nVarsPtr
        nAtts      <- fromIntegral <$> peek nAttsPtr
        unlimDimId <- (\x -> if x == (-1) then Nothing else Just (fromIntegral x)) <$> peek unlimDimIdPtr
        return $! (fromIntegral res, NCInfo nDims nVars nAtts unlimDimId)

nc_inq_format :: NC FileId -> IO (Int32, NCFormat)
nc_inq_format (NCFile ncid) =
    alloca $ \ncFormatPtr -> do
        res <- c_nc_inq_format ncid ncFormatPtr
        ncFormat <- fromNCInqFormatTag <$> peek ncFormatPtr
        case ncFormat of
            Just format -> return $! (fromIntegral res, format)
            Nothing -> return (ncErrorOrUnexpected res, undefined)

nc_inq_format_extended :: NC FileId -> IO (Int32, (NCFormatX, NCOpenMode))
nc_inq_format_extended (NCFile ncid) =
    alloca $ \ncFormatPtr ->
    alloca $ \ncModePtr -> do
        res <- c_nc_inq_format_extended ncid ncFormatPtr ncModePtr
        ncFormat <- fromNCFormatXTag <$> peek ncFormatPtr
        mode <- fromNCOpenModeTag <$> peek ncModePtr
        case ncFormat of
            Just format -> return $! (fromIntegral res, (format, mode))
            Nothing -> return (ncErrorOrUnexpected res, undefined)

nc_inq_path :: NC FileId -> IO (Int32, String)
nc_inq_path (NCFile ncid) = do
    (res1, pathLen) <- alloca $ \pathLenPtr -> do
        res <- c_nc_inq_path ncid pathLenPtr nullPtr
        ((,) res) <$> peek pathLenPtr
    if res1 /= 0
        then return (fromIntegral res1, "")
        else
            allocaArray0 (fromIntegral pathLen) $ \pathPtr -> do
                res2 <- c_nc_inq_path ncid nullPtr pathPtr
                path <- peekCString pathPtr
                return $! (fromIntegral res2, path)

nc_set_fill :: NC FileId -> NCFillMode -> IO (Int32, NCFillMode)
nc_set_fill (NCFile ncid) fillMode =
    alloca $ \oldModePtr -> do
        res <- c_nc_set_fill ncid (toNCFillTag fillMode) oldModePtr
        oldMode <- fromNCFillTag <$> peek oldModePtr
        case oldMode of
            Just mode -> return $! (fromIntegral res, mode)
            Nothing -> return (ncErrorOrUnexpected res, undefined)

ncErrorOrUnexpected :: CInt -> Int32
ncErrorOrUnexpected e = fromIntegral $ if e /= 0 then e else (toNCErrorCode NC_UNEXPECTED)
