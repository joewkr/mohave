{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Format.HDF.LowLevel.SD where

import           Data.Int
import           Data.Word
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable (peek)

import           Data.Format.HDF.LowLevel.C.Definitions

-- Access
foreign import ccall unsafe "SDstart" c_sdstart :: CString -> Int32 -> IO Int32
foreign import ccall unsafe "SDcreate" c_sdcreate :: Int32 -> CString -> Int32 -> Int32 -> Ptr Int32 -> IO Int32
foreign import ccall unsafe "SDselect" c_sdselect :: Int32 -> Int32 -> IO Int32
foreign import ccall unsafe "SDendaccess" c_sdendaccess :: Int32 -> IO CInt
foreign import ccall unsafe "SDend" c_sdend :: Int32 -> IO CInt

-- Read and write
-- SDreaddata
-- SDwritedata

-- General inquiry
foreign import ccall unsafe "SDcheckempty" c_sdcheckempty :: Int32 -> Ptr CInt -> IO Int32
foreign import ccall unsafe "SDfileinfo" c_sdfileinfo :: Int32 -> Ptr Int32 -> Ptr Int32 -> IO CInt
foreign import ccall unsafe "SDgetfilename" c_sdgetfilename :: Int32 -> CString -> IO CInt
foreign import ccall unsafe "SDgetinfo" c_sdgetinfo :: Int32 -> CString -> Ptr Int32 -> Ptr Int32 -> Ptr Int32 -> Ptr Int32 -> IO CInt
foreign import ccall unsafe "SDget_maxopenfiles" c_sdget_maxopenfiles :: Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "SDgetnamelen" c_sdgetnamelen :: Int32 -> Ptr Word16 -> IO CInt
foreign import ccall unsafe "SDget_numopenfiles" c_sdget_numopenfiles :: IO CInt
foreign import ccall unsafe "SDgetnumvars_byname" c_sdgetnumvars_byname :: Int32 -> CString -> Ptr Int32 -> IO CInt
foreign import ccall unsafe "SDidtoref" c_sdidtoref :: Int32 -> IO Int32
-- SDidtype {- no use for this function in Haskell interface -}
foreign import ccall unsafe "SDiscoordvar" c_sdiscoordvar :: Int32 -> IO CInt
foreign import ccall unsafe "SDisrecord" c_sdisrecord :: Int32 -> IO Int32
foreign import ccall unsafe "SDnametoindex" c_sdnametoindex :: Int32 -> CString -> IO Int32
foreign import ccall unsafe "SDnametoindices" c_sdnametoindices :: Int32 -> CString -> Ptr HDFVarList -> IO CInt
foreign import ccall unsafe "SDreftoindex" c_sdreftoindex :: Int32 -> Int32 -> IO Int32
foreign import ccall unsafe "SDreset_maxopenfiles" c_sdreset_maxopenfiles :: CInt -> IO CInt

-- Dimensions
foreign import ccall unsafe "SDdiminfo" c_sddiminfo :: Int32 -> CString -> Ptr Int32 -> Ptr Int32 -> Ptr Int32 -> IO CInt
foreign import ccall unsafe "SDgetdimid" c_sdgetdimid :: Int32 -> CInt -> IO Int32
foreign import ccall unsafe "SDsetdimname" c_sdsetdimname :: Int32 -> CString -> IO CInt

-- Dimension scales
-- SDgetdimscale
-- SDsetdimscale

-- User-defined attributes
-- SDattrinfo
-- SDfindattr
-- SDreadattr
-- SDsetattr

-- Predefined attributes
-- SDgetcal
-- SDgetdatastrs
-- SDgetdimstrs
-- SDgetfillvalue
-- SDgetrange
-- SDsetcal
-- SDsetdatastrs
-- SDsetdimstrs
-- SDsetfillvalue
-- SDsetfillmode
-- SDsetrange

-- Compression
-- SDsetcompress
-- SDsetnbitdataset
-- SDgetcompinfo

-- Chunking/Tiling
-- SDgetchunkinfo
-- SDreadchunk
-- SDsetchunk
-- SDsetchunkcache
-- SDwritechunk

-- Raw data information
-- SDgetanndatainfo
-- SDgetattdatainfo
-- SDgetdatainfo
-- SDgetoldattdatainfo

-- Miscellaneous
-- SDgetexternalinfo
-- SDsetblocksize
-- SDsetexternalfile
-- SDisdimval_bwcomp
-- SDsetdimval_comp
-- SDsetaccesstype

newtype SDId = SDId Int32 deriving Eq
newtype SDataSetId = SDataSetId Int32 deriving Eq
newtype SDataSetRef = SDataSetRef Int32 deriving Eq
newtype SDimensionId = SDimensionId Int32 deriving Eq

class SDObjectId id where
    getRawObjectId :: id -> Int32

instance SDObjectId SDId where
    getRawObjectId (SDId sd_id) = sd_id

instance SDObjectId SDataSetId where
    getRawObjectId (SDataSetId sds_id) = sds_id

instance SDObjectId SDimensionId where
    getRawObjectId (SDimensionId dimension_id) = dimension_id

data HdfStatus = Succeed | Fail deriving Eq



sd_start :: String -> HDFOpenOption -> IO (Int32, SDId)
sd_start fileName mode = withCString fileName $ \c_fileName -> do
    sd_id <- c_sdstart c_fileName (unHDFOpenOption mode)
    return $! (sd_id, SDId sd_id)

sd_create :: SDId -> String -> HDFDataType -> [Int32] -> IO (Int32, SDataSetId)
sd_create (SDId sd_id) sds_name data_type dim_sizes =
    withCString sds_name $ \c_sds_name ->
    withArray dim_sizes $ \c_dim_sizes -> do
        sds_id <- c_sdcreate sd_id c_sds_name (unHDFDataType data_type) rank c_dim_sizes
        return $! (sds_id, SDataSetId sds_id)
  where
    rank = fromIntegral $! length dim_sizes

sd_select :: SDId -> Int32 -> IO (Int32, SDataSetId)
sd_select (SDId sd_id) sds_index = do
    sds_id <- c_sdselect sd_id sds_index
    return $! (sds_id, SDataSetId sds_id)

sd_endaccess :: SDataSetId -> IO (Int32, ())
sd_endaccess (SDataSetId sds_id) = do
    h_result <- c_sdendaccess sds_id
    return $! (fromIntegral h_result, ())

sd_end :: SDId -> IO (Int32, ())
sd_end (SDId sd_id) = do
    h_result <- c_sdend sd_id
    return $! (fromIntegral h_result, ())

sd_checkempty :: SDataSetId -> IO (Int32, Bool)
sd_checkempty (SDataSetId sds_id) = alloca $ \emptySDSPtr -> do
    h_result <- c_sdcheckempty sds_id emptySDSPtr
    res <- if h_result == (-1)
        then return (-1)
        else peek emptySDSPtr
    return $! (h_result, res /= 0)

sd_fileinfo :: SDId -> IO (Int32, (Int32, Int32))
sd_fileinfo (SDId sd_id) =
    alloca $ \numSDSPtr ->
    alloca $ \numAttributesPtr -> do
        h_result <- c_sdfileinfo sd_id numSDSPtr numAttributesPtr
        res <- if h_result == (-1)
            then return (0, 0)
            else do
                numSDS <- peek numSDSPtr
                numAttributes <- peek numAttributesPtr
                return $! (numSDS, numAttributes)
        return $! (fromIntegral h_result, res)

sd_getnamelen :: SDObjectId id => id -> IO (Int32, Int32)
sd_getnamelen objId = alloca $ \nameLenPtr -> do
    h_result <- c_sdgetnamelen (getRawObjectId objId) nameLenPtr
    res <- if h_result == (-1)
        then return 0
        else peek nameLenPtr
    return $! (fromIntegral h_result, fromIntegral res)

sd_getfilename :: SDId -> IO (Int32, String)
sd_getfilename sdFileId@(SDId sd_id) = do
    (h_result_getnamelen, nameLen) <- sd_getnamelen sdFileId
    if h_result_getnamelen == (-1)
        then return (-1, "")
        else allocaArray (fromIntegral $ nameLen + 1) $ \fileNamePtr -> do
            h_result <- c_sdgetfilename sd_id fileNamePtr
            fileName <- peekCString fileNamePtr
            return $! (fromIntegral h_result, fileName)

data SDataSetInfoRaw = SDataSetInfoRaw {
      sDataSetName     :: String
    , sDataSetRank     :: Int32
    , sDataSetDimSizes :: [Int32]
    , sDataSetDataType :: Int32
    , sDataSetNumAttrs :: Int32
} deriving (Show, Eq)

sd_getinfo :: SDataSetId -> IO (Int32, SDataSetInfoRaw)
sd_getinfo sDataSetId@(SDataSetId sds_id) = do
    (h_result_getnamelen, nameLen) <- sd_getnamelen sDataSetId
    if h_result_getnamelen == (-1)
        then return (-1, emptySDataSetInfo)
        else
            alloca $ \rankPtr ->
            alloca $ \dataTypePtr ->
            alloca $ \numAttributesPtr ->
            allocaArray (fromIntegral $ nameLen + 1) $ \sdsNamePtr -> do
            allocaArray (fromIntegral hdfMaxVarDims) $ \dimSizesPtr -> do
                h_result <- c_sdgetinfo
                                sds_id
                                sdsNamePtr
                                rankPtr
                                dimSizesPtr
                                dataTypePtr
                                numAttributesPtr
                sdsName <- peekCString sdsNamePtr
                rank <- peek rankPtr
                dimSizes <- peekArray (fromIntegral rank) dimSizesPtr
                dataType <- peek dataTypePtr
                numAttributes <- peek numAttributesPtr
                return $! (fromIntegral h_result, SDataSetInfoRaw sdsName rank dimSizes dataType numAttributes)

  where
    emptySDataSetInfo :: SDataSetInfoRaw
    emptySDataSetInfo = SDataSetInfoRaw "" 0 [] 0 0

sd_get_maxopenfiles :: IO (Int32, (Int32, Int32))
sd_get_maxopenfiles =
    alloca $ \currMaxPtr ->
    alloca $ \sysLimitPtr -> do
        h_result <- c_sdget_maxopenfiles currMaxPtr sysLimitPtr
        currMax <- peek currMaxPtr
        sysLimit <- peek sysLimitPtr
        return $! (fromIntegral h_result, (fromIntegral currMax, fromIntegral sysLimit))

sd_get_numopenfiles :: IO (Int32, Int32)
sd_get_numopenfiles = do
    h_result <- c_sdget_numopenfiles
    return $! (fromIntegral h_result, fromIntegral h_result)

sd_getnumvars_byname :: SDId -> String -> IO (Int32, Int32)
sd_getnumvars_byname (SDId sd_id) sds_name =
    withCString sds_name $ \cSDSName -> do
    alloca $ \numVarsPtr -> do
        h_result <- c_sdgetnumvars_byname sd_id cSDSName numVarsPtr
        if h_result == (-1)
            then return (-1, 0)
            else do
                numVars <- peek numVarsPtr
                return $! (fromIntegral h_result, fromIntegral numVars)

sd_idtoref :: SDataSetId -> IO (Int32, SDataSetRef)
sd_idtoref (SDataSetId sds_id) = do
    sds_ref <- c_sdidtoref sds_id
    return $! (sds_ref, SDataSetRef sds_ref)

sd_iscoordvar :: SDataSetId -> IO (Int32, Bool)
sd_iscoordvar (SDataSetId sds_id) = do
    is_coordvar <- c_sdiscoordvar sds_id
    return $! (fromIntegral is_coordvar, is_coordvar /= 0)

sd_isisrecord :: SDataSetId -> IO (Int32, Bool)
sd_isisrecord (SDataSetId sds_id) = do
    is_record <- c_sdisrecord sds_id
    return $! (fromIntegral is_record, is_record /= 0)

sd_nametoindex :: SDId -> String -> IO (Int32, Int32)
sd_nametoindex (SDId sd_id) sds_name = withCString sds_name $ \cSDSName -> do
    sds_index <- c_sdnametoindex sd_id cSDSName
    return $! (sds_index, sds_index)

sd_nametoindices :: SDId -> String -> IO (Int32, [HDFVarList])
sd_nametoindices sd@(SDId sd_id) sds_name = do
    (h_result_getnumvars_byname, numVars) <- sd_getnumvars_byname sd sds_name
    if h_result_getnumvars_byname == (-1)
        then return $! (h_result_getnumvars_byname, [])
        else
            withCString sds_name $ \cSDSName ->
            allocaArray (fromIntegral numVars) $ \sdsVarListPtr -> do
                h_result <- c_sdnametoindices sd_id cSDSName sdsVarListPtr
                sdsVarList <- peekArray (fromIntegral numVars) sdsVarListPtr
                return $! (fromIntegral h_result, sdsVarList)


sd_reftoindex :: SDId -> SDataSetRef -> IO (Int32, Int32)
sd_reftoindex (SDId sd_id) (SDataSetRef sds_ref) = do
    sds_index <- c_sdreftoindex sd_id sds_ref
    return $! (sds_index, sds_index)

sd_reset_maxopenfiles :: Int32 -> IO (Int32, Int32)
sd_reset_maxopenfiles newLimit = do
    current_limit <- c_sdreset_maxopenfiles (fromIntegral newLimit)
    return $! (fromIntegral current_limit, fromIntegral current_limit)

sd_getdimid :: SDataSetId -> Int32 -> IO (Int32, SDimensionId)
sd_getdimid (SDataSetId sds_id) dim_index = do
    dimension_id <- c_sdgetdimid sds_id (fromIntegral dim_index)
    return $! (dimension_id, SDimensionId dimension_id)

data SDimensionInfoRaw = SDimensionInfoRaw {
      sDimensionName     :: String
    , sDimensionSize     :: Int32
    , sDimensionDataType :: Int32
    , sDimensionNumAttrs :: Int32
} deriving (Show, Eq)

sd_diminfo :: SDimensionId -> IO (Int32, SDimensionInfoRaw)
sd_diminfo sDimensionId@(SDimensionId dimension_id) = do
    (h_result_getnamelen, nameLen) <- sd_getnamelen sDimensionId
    if h_result_getnamelen == (-1)
        then return (-1, emptySDimensionInfo)
        else
            alloca $ \dimSizePtr ->
            alloca $ \dataTypePtr ->
            alloca $ \numAttributesPtr ->
            allocaArray (fromIntegral $ nameLen + 1) $ \dimNamePtr -> do
                h_result <- c_sddiminfo
                                dimension_id
                                dimNamePtr
                                dimSizePtr
                                dataTypePtr
                                numAttributesPtr
                dimName <- peekCString dimNamePtr
                dimSize <- peek dimSizePtr
                dataType <- peek dataTypePtr
                numAttributes <- peek numAttributesPtr
                return $! (fromIntegral h_result, SDimensionInfoRaw dimName dimSize dataType numAttributes)
  where
    emptySDimensionInfo :: SDimensionInfoRaw
    emptySDimensionInfo = SDimensionInfoRaw "" 0 0 0

sd_setdimname :: SDimensionId -> String -> IO (Int32, ())
sd_setdimname (SDimensionId dimension_id) dimension_name =
    withCString dimension_name $ \c_dimension_name -> do
        h_result <- c_sdsetdimname dimension_id c_dimension_name
        return $! (fromIntegral h_result, ())
