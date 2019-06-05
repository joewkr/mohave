{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Format.HDF.LowLevel.SD where

import           Data.Int
import           Data.Word
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils (with, fromBool)
import           Foreign.Ptr
import           Foreign.Storable (peek)

import           Data.Format.HDF.LowLevel.C.Definitions
import           Data.Format.HDF.LowLevel.Definitions

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
foreign import ccall unsafe "SDattrinfo" c_sdattrinfo :: Int32 -> Int32 -> CString -> Ptr Int32 -> Ptr Int32 -> IO CInt
foreign import ccall unsafe "SDfindattr" c_sdfindattr :: Int32 -> CString -> IO Int32
-- SDreadattr
-- SDsetattr

-- Predefined attributes
foreign import ccall unsafe "SDgetcal" c_sdgetcal :: Int32 -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Int32 -> IO CInt
foreign import ccall unsafe "SDgetdatastrs" c_sdgetdatastrs :: Int32 -> CString -> CString -> CString -> CString -> CInt -> IO CInt
foreign import ccall unsafe "SDgetdimstrs" c_sdgetdimstrs :: Int32 -> CString -> CString -> CString -> CInt -> IO CInt
foreign import ccall unsafe "SDgetfillvalue" c_sdgetfillvalue :: Int32 -> Ptr HDFData -> IO CInt
foreign import ccall unsafe "SDgetrange" c_sdgetrange :: Int32 -> Ptr HDFData -> Ptr HDFData -> IO CInt
foreign import ccall unsafe "SDsetcal" c_sdsetcal :: Int32 -> Double -> Double -> Double -> Double -> Int32 -> IO CInt
foreign import ccall unsafe "SDsetdatastrs" c_sdsetdatastrs :: Int32 -> CString -> CString -> CString -> CString -> IO CInt
foreign import ccall unsafe "SDsetdimstrs" c_sdsetdimstrs :: Int32 -> CString -> CString -> CString -> IO CInt
foreign import ccall unsafe "SDsetfillvalue" c_sdsetfillvalue :: Int32 -> Ptr HDFData -> IO CInt
foreign import ccall unsafe "SDsetfillmode" c_sdsetfillmode :: Int32 -> CInt -> IO CInt
foreign import ccall unsafe "SDsetrange" c_sdsetrange :: Int32 -> Ptr HDFData -> Ptr HDFData -> IO CInt

-- Compression
foreign import ccall unsafe "SDsetcompress" c_sdsetcompress :: Int32 -> HDFCompType -> Ptr HDFCompParams -> IO CInt
foreign import ccall unsafe "SDsetnbitdataset" c_sdsetnbitdataset :: Int32 -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "SDgetcompinfo" c_sdgetcompinfo :: Int32 -> Ptr HDFCompType -> Ptr HDFCompParams -> IO CInt

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

sd_create :: SDId -> String -> HDFDataTypeTag -> [Int32] -> IO (Int32, SDataSetId)
sd_create (SDId sd_id) sds_name data_type dim_sizes =
    withCString sds_name $ \c_sds_name ->
    withArray dim_sizes $ \c_dim_sizes -> do
        sds_id <- c_sdcreate sd_id c_sds_name (unHDFDataTypeTag data_type) rank c_dim_sizes
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
                return $!
                    ( fromIntegral h_result
                    , SDataSetInfoRaw
                        sdsName
                        rank
                        dimSizes
                        dataType
                        numAttributes)
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
                return $!
                    ( fromIntegral h_result
                    , SDimensionInfoRaw
                        dimName
                        dimSize
                        dataType
                        numAttributes)
  where
    emptySDimensionInfo :: SDimensionInfoRaw
    emptySDimensionInfo = SDimensionInfoRaw "" 0 0 0

sd_setdimname :: SDimensionId -> String -> IO (Int32, ())
sd_setdimname (SDimensionId dimension_id) dimension_name =
    withCString dimension_name $ \c_dimension_name -> do
        h_result <- c_sdsetdimname dimension_id c_dimension_name
        return $! (fromIntegral h_result, ())

sd_findattr :: SDObjectId id => id -> String -> IO (Int32, Int32)
sd_findattr objId attribute_name =
    withCString attribute_name $ \c_attribute_name -> do
        attribute_index <- c_sdfindattr (getRawObjectId objId) c_attribute_name
        return (attribute_index, attribute_index)

data SAttributeInfoRaw = SAttributeInfoRaw {
      sAttributeName     :: String
    , sAttributeNValues  :: Int32
    , sAttributeDataType :: Int32
} deriving (Show, Eq)

sd_attrinfo :: SDObjectId id => id -> Int32 -> IO (Int32, SAttributeInfoRaw)
sd_attrinfo objId attrId =
    alloca $ \attrNValuesPtr ->
    alloca $ \dataTypePtr ->
    allocaArray (fromIntegral $ hdfMaxNcNameLen + 1) $ \attrNamePtr -> do
        h_result <- c_sdattrinfo
                        (getRawObjectId objId)
                        attrId
                        attrNamePtr
                        dataTypePtr
                        attrNValuesPtr
        if h_result == (-1)
            then return $! (fromIntegral h_result, emptySAttributeInfo)
            else do
                attrName    <- peekCString attrNamePtr
                attrNValues <- peek attrNValuesPtr
                dataType    <- peek dataTypePtr
                return $!
                    ( fromIntegral h_result
                    , SAttributeInfoRaw
                        attrName
                        attrNValues
                        dataType)
  where
    emptySAttributeInfo :: SAttributeInfoRaw
    emptySAttributeInfo = SAttributeInfoRaw "" 0 0

data SCalibrationParametersRaw = SCalibrationParametersRaw {
      sCalibrationFactor       :: Double
    , sCalibrationScalingError :: Double
    , sUncalibratedOffset      :: Double
    , sCalibrationOffsetError  :: Double
    , sUncalibratedDataType    :: Int32
} deriving (Show, Eq)

sd_getcal :: SDataSetId -> IO (Int32, SCalibrationParametersRaw)
sd_getcal (SDataSetId sds_id) =
    alloca $ \calibrationFactorPtr ->
    alloca $ \calibrationScalingErrorPtr ->
    alloca $ \uncalibratedOffsetPtr ->
    alloca $ \calibrationOffsetErrorPtr ->
    alloca $ \uncalibratedDataTypePtr -> do
        h_result <- c_sdgetcal
                        sds_id
                        calibrationFactorPtr
                        calibrationScalingErrorPtr
                        uncalibratedOffsetPtr
                        calibrationOffsetErrorPtr
                        uncalibratedDataTypePtr
        if h_result == (-1)
            then return $! (fromIntegral h_result, emptySCalibrationParameters)
            else do
                calibrationFactor       <- peek calibrationFactorPtr
                calibrationScalingError <- peek calibrationScalingErrorPtr
                uncalibratedOffset      <- peek uncalibratedOffsetPtr
                calibrationOffsetError  <- peek calibrationOffsetErrorPtr
                uncalibratedDataType    <- peek uncalibratedDataTypePtr
                return $!
                    ( fromIntegral h_result
                    , SCalibrationParametersRaw
                        calibrationFactor
                        calibrationScalingError
                        uncalibratedOffset
                        calibrationOffsetError
                        uncalibratedDataType)
  where
    emptySCalibrationParameters :: SCalibrationParametersRaw
    emptySCalibrationParameters = SCalibrationParametersRaw 1.0 0.0 0.0 0.0 0

data SDsetDescStringsRaw = SDsetDescStringsRaw {
      sDSLabel            :: String
    , sDSUnit             :: String
    , sDSFormat           :: String
    , sDSCoordinateSystem :: String
} deriving (Show, Eq)

sd_getdatastrs :: SDataSetId -> IO (Int32, SDsetDescStringsRaw)
sd_getdatastrs (SDataSetId sds_id) =
    allocaArray (maxBufferLength) $ \labelPtr ->
    allocaArray (maxBufferLength) $ \unitPtr ->
    allocaArray (maxBufferLength) $ \formatPtr ->
    allocaArray (maxBufferLength) $ \coordinateSystemPtr -> do
        h_result <- c_sdgetdatastrs
                        sds_id
                        labelPtr
                        unitPtr
                        formatPtr
                        coordinateSystemPtr
                        (fromIntegral maxBufferLength)
        if h_result == (-1)
            then return $! (fromIntegral h_result, emptySDsetDescStrings)
            else do
                label            <- peekCString labelPtr
                unit             <- peekCString unitPtr
                format           <- peekCString formatPtr
                coordinateSystem <- peekCString coordinateSystemPtr
                return $!
                    ( fromIntegral h_result
                    , SDsetDescStringsRaw
                        label
                        unit
                        format
                        coordinateSystem)
  where
    maxBufferLength :: Int
    maxBufferLength = 1024

    emptySDsetDescStrings :: SDsetDescStringsRaw
    emptySDsetDescStrings = SDsetDescStringsRaw "" "" "" ""

data SDimDescStringsRaw = SDimDescStringsRaw {
      sDimLabel            :: String
    , sDimUnit             :: String
    , sDimFormat           :: String
} deriving (Show, Eq)

sd_getdimstrs :: SDimensionId -> IO (Int32, SDimDescStringsRaw)
sd_getdimstrs (SDimensionId dimension_id) =
    allocaArray (maxBufferLength) $ \labelPtr ->
    allocaArray (maxBufferLength) $ \unitPtr ->
    allocaArray (maxBufferLength) $ \formatPtr -> do
        h_result <- c_sdgetdimstrs
                        dimension_id
                        labelPtr
                        unitPtr
                        formatPtr
                        (fromIntegral maxBufferLength)
        if h_result == (-1)
            then return $! (fromIntegral h_result, emptySDimDescStrings)
            else do
                label            <- peekCString labelPtr
                unit             <- peekCString unitPtr
                format           <- peekCString formatPtr
                return $!
                    ( fromIntegral h_result
                    , SDimDescStringsRaw
                        label
                        unit
                        format)
  where
    maxBufferLength :: Int
    maxBufferLength = 1024

    emptySDimDescStrings :: SDimDescStringsRaw
    emptySDimDescStrings = SDimDescStringsRaw "" "" ""

sd_getfillvalue :: HDFDataType a => SDataSetId -> IO (Int32, a)
sd_getfillvalue (SDataSetId sds_id) =
    alloca $ \fillValuePtr -> do
        h_result <- c_sdgetfillvalue sds_id (castPtr fillValuePtr)
        fillValue <- peek fillValuePtr
        return $! (fromIntegral h_result, fillValue)

sd_getrange :: HDFDataType a => SDataSetId -> IO (Int32, (a, a))
sd_getrange (SDataSetId sds_id) =
    alloca $ \minValuePtr ->
    alloca $ \maxValuePtr -> do
        h_result <- c_sdgetrange sds_id (castPtr maxValuePtr) (castPtr minValuePtr)
        minValue <- peek minValuePtr
        maxValue <- peek maxValuePtr
        return $! (fromIntegral h_result, (minValue, maxValue))

sd_setcal :: SDataSetId -> SCalibrationParametersRaw -> IO (Int32, ())
sd_setcal (SDataSetId sds_id) (SCalibrationParametersRaw s sE o oE dtype) = do
    h_result <- c_sdsetcal sds_id s sE o oE dtype
    return $! (fromIntegral h_result, ())

withCStringOrNull :: String -> (CString -> IO a) -> IO a
withCStringOrNull []  fun = fun nullPtr
withCStringOrNull str fun = withCString str fun

sd_setdatastrs :: SDataSetId -> SDsetDescStringsRaw -> IO (Int32, ())
sd_setdatastrs (SDataSetId sds_id) (SDsetDescStringsRaw l u f c) =
    withCStringOrNull l $ \lPtr ->
    withCStringOrNull u $ \uPtr ->
    withCStringOrNull f $ \fPtr ->
    withCStringOrNull c $ \cPtr -> do
        h_result <- c_sdsetdatastrs sds_id lPtr uPtr fPtr cPtr
        return $! (fromIntegral h_result, ())

sd_setdimstrs :: SDimensionId -> SDimDescStringsRaw -> IO (Int32, ())
sd_setdimstrs (SDimensionId dimension_id) (SDimDescStringsRaw l u f) =
    withCStringOrNull l $ \lPtr ->
    withCStringOrNull u $ \uPtr ->
    withCStringOrNull f $ \fPtr -> do
        h_result <- c_sdsetdimstrs dimension_id lPtr uPtr fPtr
        return $! (fromIntegral h_result, ())

sd_setfillvalue :: HDFDataType a => SDataSetId -> a -> IO (Int32, ())
sd_setfillvalue (SDataSetId sds_id) fillValue =
    with fillValue $ \fillValuePtr -> do
        h_result <- c_sdsetfillvalue sds_id (castPtr fillValuePtr)
        return $! (fromIntegral h_result, ())

sd_setfillmode :: SDataSetId -> HDFFillModeTag -> IO (Int32, ())
sd_setfillmode (SDataSetId sds_id) fillMode = do
    h_result <- c_sdsetfillmode sds_id (fromIntegral . unHDFFillModeTag $ fillMode)
    return $! (fromIntegral h_result, ())

sd_setrange :: HDFDataType a => SDataSetId -> a -> a -> IO (Int32, ())
sd_setrange (SDataSetId sds_id) minValue maxValue =
    with minValue $ \minValuePtr ->
    with maxValue $ \maxValuePtr -> do
        h_result <- c_sdsetrange sds_id (castPtr maxValuePtr) (castPtr minValuePtr)
        return $! (fromIntegral h_result, ())

sd_setcompress :: SDataSetId -> HDFCompParams -> IO (Int32, ())
sd_setcompress (SDataSetId sds_id) compParams =
    with compParams $ \compParamsPtr -> do
        h_result <- c_sdsetcompress
                        sds_id
                        (unHDFCompModeTag $ selectCompMode compParams)
                        compParamsPtr
        return $! (fromIntegral h_result, ())

sd_getcompinfo :: SDataSetId -> IO (Int32, HDFCompParams)
sd_getcompinfo (SDataSetId sds_id) =
    alloca $ \compTypePtr ->
    alloca $ \compParamsPtr -> do
        h_result <- c_sdgetcompinfo sds_id compTypePtr compParamsPtr
        peek compTypePtr >>= embedCompTag compParamsPtr
        compParams <- peek compParamsPtr
        return $! (fromIntegral h_result, compParams)

data SDNBitCompParams = SDNBitCompParams {
      nBitCompStartBit :: Int32
    , nBitCompBitLen   :: Int32
    , nBitCompSignExt  :: Bool
    , nBitCompFillOne  :: Bool
} deriving (Show, Eq)

sd_setnbitdataset :: SDataSetId -> SDNBitCompParams -> IO (Int32, ())
sd_setnbitdataset
    (SDataSetId sds_id)
    (SDNBitCompParams
        startBit
        bitLen
        signExt
        fillOne) = do
    h_result <- c_sdsetnbitdataset
                    sds_id
                    (fromIntegral startBit)
                    (fromIntegral bitLen)
                    (fromBool signExt)
                    (fromBool fillOne)
    return $! (fromIntegral h_result, ())
