{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Format.NetCDF.LowLevel.Group where

import           Data.Int
import           Foreign.Ptr
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils (fromBool)
import           Foreign.Storable

import           Data.Format.NetCDF.LowLevel.Definitions
import           Data.Format.NetCDF.LowLevel.C.Definitions
import           Data.Format.NetCDF.LowLevel.Variable.Internal (mkSomeNCVariable)

foreign import ccall unsafe "nc_def_grp" c_nc_def_grp :: CInt -> CString -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_dimids" c_nc_inq_dimids :: CInt -> Ptr CInt -> Ptr CInt -> Int -> IO CInt
foreign import ccall unsafe "nc_inq_grp_full_ncid" c_nc_inq_grp_full_ncid :: CInt -> CString -> Ptr CInt -> IO CInt
{- Just an alias for nc_inq_ncid -}
-- int     nc_inq_grp_ncid (int ncid, const char *grp_name, int *grp_ncid)
foreign import ccall unsafe "nc_inq_grp_parent" c_nc_inq_grp_parent :: CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_grpname" c_nc_inq_grpname :: CInt -> CString -> IO CInt
foreign import ccall unsafe "nc_inq_grpname_full" c_nc_inq_grpname_full :: CInt -> Ptr CInt -> CString -> IO CInt
foreign import ccall unsafe "nc_inq_grpname_len" c_nc_inq_grpname_len :: CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_grps" c_nc_inq_grps :: CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_inq_ncid" c_nc_inq_ncid :: CInt -> CString -> Ptr CInt -> IO CInt
{- No Haskell-side support for user-defined types yet, so this function is not very useful -}
-- int     nc_inq_typeids (int ncid, int *ntypes, int *typeids)
foreign import ccall unsafe "nc_inq_varids" c_nc_inq_varids :: CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "nc_rename_grp" c_nc_rename_grp :: CInt -> CString -> IO CInt
-- int     nc_show_metadata (int ncid)

nc_def_grp :: NC id -> String -> IO (Int32, NC GroupId)
nc_def_grp parentNcid name =
    withCString name $ \c_name ->
    alloca $ \groupNcidPtr -> do
        res <- c_nc_def_grp (ncRawId parentNcid) c_name groupNcidPtr
        groupNcid <- NCGroup <$> peek groupNcidPtr
        return $! (fromIntegral res, groupNcid)

nc_inq_dimids :: NC id -> Bool -> IO (Int32, [NCDimensionId])
nc_inq_dimids ncid includeParents = alloca $ \numDimsPtr -> do
    res1 <- c_nc_inq_dimids (ncRawId ncid) numDimsPtr nullPtr (fromBool includeParents)
    if res1 /= 0
        then return $! (fromIntegral res1, [])
        else do
            numDims <- peek numDimsPtr
            allocaArray (fromIntegral numDims) $ \dimIdsPtr -> do
                res <- c_nc_inq_dimids (ncRawId ncid) nullPtr dimIdsPtr (fromBool includeParents)
                dimIds <- map NCDimensionId <$> peekArray (fromIntegral numDims) dimIdsPtr
                return $! (fromIntegral res, dimIds)

nc_inq_grp_full_ncid :: NC id -> String -> IO (Int32, NC GroupId)
nc_inq_grp_full_ncid ncid fullName =
    withCString fullName $ \c_fullName ->
    alloca $ \groupNcidPtr -> do
        res <- c_nc_inq_grp_full_ncid (ncRawId ncid) c_fullName groupNcidPtr
        groupNcid <- NCGroup <$> peek groupNcidPtr
        return $! (fromIntegral res, groupNcid)

nc_inq_ncid :: NC id -> String -> IO (Int32, NC GroupId)
nc_inq_ncid ncid name =
    withCString name $ \c_name ->
    alloca $ \groupNcidPtr -> do
        res <- c_nc_inq_ncid (ncRawId ncid) c_name groupNcidPtr
        groupNcid <- NCGroup <$> peek groupNcidPtr
        return $! (fromIntegral res, groupNcid)

nc_inq_grp_ncid :: NC id -> String -> IO (Int32, NC GroupId)
nc_inq_grp_ncid = nc_inq_ncid

nc_inq_grp_parent :: NC GroupId -> IO (Int32, NC GroupId)
nc_inq_grp_parent ncid =
    alloca $ \groupNcidPtr -> do
        res <- c_nc_inq_grp_parent (ncRawId ncid) groupNcidPtr
        groupNcid <- NCGroup <$> peek groupNcidPtr
        return $! (fromIntegral res, groupNcid)

nc_inq_grpname :: NC id -> IO (Int32, String)
nc_inq_grpname ncid =
    allocaArray0 (fromIntegral ncMaxNameLen) $ \c_grpName -> do
        res <- c_nc_inq_grpname (ncRawId ncid) c_grpName
        grpName <- peekCString c_grpName
        return $! (fromIntegral res, grpName)

nc_inq_grpname_full :: NC id -> IO (Int32, String)
nc_inq_grpname_full ncid = alloca $ \grpNameLenPtr -> do
    res1 <- c_nc_inq_grpname_full (ncRawId ncid) grpNameLenPtr nullPtr
    if res1 /= 0
        then return $! (fromIntegral res1, "")
        else do
            grpNameLen <- peek grpNameLenPtr
            allocaArray0 (fromIntegral grpNameLen) $ \c_grpName -> do
                res <- c_nc_inq_grpname_full (ncRawId ncid) nullPtr c_grpName
                grpName <- peekCString c_grpName
                return $! (fromIntegral res, grpName)

nc_inq_grpname_len :: NC id -> IO (Int32, Int)
nc_inq_grpname_len ncid =
    alloca $ \grpNameLenPtr -> do
        res <- c_nc_inq_grpname_len (ncRawId ncid) grpNameLenPtr
        grpNameLen <- peek grpNameLenPtr
        return $! (fromIntegral res, fromIntegral grpNameLen)

nc_inq_grps :: NC id -> IO (Int32, [NC GroupId])
nc_inq_grps ncid = alloca $ \numGrpPtr -> do
    res1 <- c_nc_inq_grps (ncRawId ncid) numGrpPtr nullPtr
    if res1 /= 0
        then return $! (fromIntegral res1, [])
        else do
            numGrp <- peek numGrpPtr
            allocaArray (fromIntegral numGrp) $ \groupNcidsPtr -> do
                res <- c_nc_inq_grps (ncRawId ncid) nullPtr groupNcidsPtr
                grpIds <- map NCGroup <$> peekArray (fromIntegral numGrp) groupNcidsPtr
                return $! (fromIntegral res, grpIds)

nc_inq_varids :: NC id -> IO (Int32, [SomeNCVariable])
nc_inq_varids ncid = alloca $ \numVarPtr -> do
    res1 <- c_nc_inq_varids (ncRawId ncid) numVarPtr nullPtr
    if res1 /= 0
        then return $! (fromIntegral res1, [])
        else do
            numVar <- peek numVarPtr
            allocaArray (fromIntegral numVar) $ \groupNcidsPtr -> do
                res <- c_nc_inq_varids (ncRawId ncid) nullPtr groupNcidsPtr
                grpIds <- mapM (mkSomeNCVariable ncid) =<< peekArray (fromIntegral numVar) groupNcidsPtr
                return $! (fromIntegral res, grpIds)

nc_rename_grp :: NC GroupId -> String -> IO (Int32, ())
nc_rename_grp ncid newName =
    withCString newName $ \c_newName -> do
        res <- c_nc_rename_grp (ncRawId ncid) c_newName
        return $! (fromIntegral res, ())
