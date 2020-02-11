{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-} -- Required with ghc 8.4.3
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Format.HDF.LowLevel.SD(
    SDId
  , SDataSetId
  , SDataSetRef
  , SDimensionId
  , SomeSDS(..)

  , HDFOpenMode(..)

  , HDFVarList(..)
  , HDFCompParams(..)
  , HDFChunkParams(..)

  , SDataSetInfoRaw(..)
  , SDimensionInfoRaw(..)
  , SAttributeInfoRaw(..)
  , SCalibrationParametersRaw(..)
  , SDsetDescStringsRaw(..)
  , SDimDescStringsRaw(..)
  , SDNBitCompParams(..)

  , RawDataInfo

-- ** Access
  , sd_start
  , sd_create
  , sd_select
  , sd_endaccess
  , sd_end
-- ** Read and write
  , sd_readdata
  , sd_writedata
-- ** General inquiry
  , sd_checkempty
  , sd_fileinfo
  , sd_get_maxopenfiles
  , sd_get_numopenfiles
  , sd_getfilename
  , sd_getinfo
  , sd_getnamelen
  , sd_getnumvars_byname
  , sd_idtoref
  , sd_iscoordvar
  , sd_isrecord
  , sd_nametoindex
  , sd_nametoindices
  , sd_reftoindex
  , sd_reset_maxopenfiles
-- ** Dimensions
  , sd_diminfo
  , sd_getdimid
  , sd_getdimscale
  , sd_setdimname
  , sd_setdimscale
-- ** User-defined attributes
  , sd_attrinfo
  , sd_findattr
  , sd_readattr
  , sd_setattr
-- ** Predefined attributes
  , sd_getcal
  , sd_getdatastrs
  , sd_getdimstrs
  , sd_getfillvalue
  , sd_getrange
  , sd_setcal
  , sd_setdatastrs
  , sd_setdimstrs
  , sd_setfillmode
  , sd_setfillvalue
  , sd_setrange
-- ** Compression
  , sd_getcompinfo
  , sd_setcompress
  , sd_setnbitdataset
-- ** Chunking/Tiling
  , sd_getchunkinfo
  , sd_readchunk
  , sd_setchunk
  , sd_setchunkcache
  , sd_writechunk
-- ** Raw data information
  , sd_getanndatainfo
  , sd_getattdatainfo
  , sd_getdatainfo
  , sd_getoldattdatainfo
-- ** Miscellaneous
  , sd_getexternalinfo
  , sd_isdimval_bwcomp
  , sd_setblocksize
  , sd_setdimval_comp
  , sd_setexternalfile
) where

import           Control.Arrow (second)
import           Data.Int
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy(..))
import qualified Data.Vector.Storable as VS
import           Data.Word
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils (with, fromBool, toBool, fillBytes)
import           Foreign.Ptr
import           Foreign.Storable (Storable, peek, poke, sizeOf)
import           GHC.TypeNats (natVal, someNatVal, SomeNat(..), Nat, KnownNat)

import           Data.Format.HDF.LowLevel.C.Definitions
import           Data.Format.HDF.LowLevel.Definitions
import           Data.Format.HDF.LowLevel.Definitions.Internal

-- Access
foreign import ccall unsafe "SDstart" c_sdstart :: CString -> Int32 -> IO Int32
foreign import ccall unsafe "SDcreate" c_sdcreate :: Int32 -> CString -> Int32 -> Int32 -> Ptr Int32 -> IO Int32
foreign import ccall unsafe "SDselect" c_sdselect :: Int32 -> Int32 -> IO Int32
foreign import ccall unsafe "SDendaccess" c_sdendaccess :: Int32 -> IO CInt
foreign import ccall unsafe "SDend" c_sdend :: Int32 -> IO CInt

-- Read and write
foreign import ccall unsafe "SDreaddata" c_sdreaddata :: Int32 -> Ptr Int32 -> Ptr Int32 -> Ptr Int32 -> Ptr HDFData -> IO CInt
foreign import ccall unsafe "SDwritedata" c_sdwritedata :: Int32 -> Ptr Int32 -> Ptr Int32 -> Ptr Int32 -> Ptr HDFData -> IO CInt

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
foreign import ccall unsafe "SDgetdimscale" c_sdgetdimscale :: Int32 -> Ptr HDFData -> IO CInt
foreign import ccall unsafe "SDsetdimscale" c_sdsetdimscale :: Int32 -> Int32 -> Int32 -> Ptr HDFData -> IO CInt

-- User-defined attributes
foreign import ccall unsafe "SDattrinfo" c_sdattrinfo :: Int32 -> Int32 -> CString -> Ptr Int32 -> Ptr Int32 -> IO CInt
foreign import ccall unsafe "SDfindattr" c_sdfindattr :: Int32 -> CString -> IO Int32
foreign import ccall unsafe "SDreadattr" c_sdreadattr :: Int32 -> Int32 -> Ptr HDFData -> IO CInt
foreign import ccall unsafe "SDsetattr" c_sdsetattr :: Int32 -> CString -> Int32 -> Int32 -> Ptr HDFData -> IO CInt

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
foreign import ccall unsafe "SDgetchunkinfo" c_sdgetchunkinfo :: Int32 -> Ptr HDFChunkParams -> Ptr Int32 -> IO CInt
foreign import ccall unsafe "SDreadchunk" c_sdreadchunk :: Int32 -> Ptr Int32 -> Ptr HDFData -> IO CInt
foreign import ccall unsafe "wrp_SDsetchunk" c_sdsetchunk :: Int32 -> Ptr HDFChunkParams -> Int32 -> IO CInt
foreign import ccall unsafe "SDsetchunkcache" c_sdsetchunkcache :: Int32 -> Int32 -> Int32 -> IO CInt
foreign import ccall unsafe "SDwritechunk" c_sdwritechunk :: Int32 -> Ptr Int32 -> Ptr HDFData -> IO CInt

-- Raw data information
foreign import ccall unsafe "SDgetanndatainfo" c_sdgetanndatainfo :: Int32 -> CAnnType -> CUInt -> Ptr Int32 -> Ptr Int32 -> IO CInt
foreign import ccall unsafe "SDgetattdatainfo" c_sdgetattdatainfo :: Int32 -> Int32 -> Ptr Int32 -> Ptr Int32 -> IO CInt
foreign import ccall unsafe "SDgetdatainfo" c_sdgetdatainfo :: Int32 -> Ptr Int32 -> CUInt -> CUInt -> Ptr Int32 -> Ptr Int32 -> IO CInt
foreign import ccall unsafe "SDgetoldattdatainfo" c_sdgetoldattdatainfo :: Int32 -> Int32 -> CString -> Ptr Int32 -> Ptr Int32 -> IO CInt

-- Miscellaneous
foreign import ccall unsafe "SDgetexternalinfo" c_sdgetexternalinfo :: Int32 -> CUInt -> CString -> Ptr Int32 -> Ptr Int32 -> IO CInt
foreign import ccall unsafe "SDsetblocksize" c_sdsetblocksize :: Int32 -> Int32 -> IO CInt
foreign import ccall unsafe "SDsetexternalfile" c_sdsetexternalfile :: Int32 -> CString -> Int32 -> IO CInt
foreign import ccall unsafe "SDisdimval_bwcomp" c_sdisdimval_bwcomp :: Int32 -> IO CInt
foreign import ccall unsafe "SDsetdimval_comp" c_sdsetdimval_comp :: Int32 -> CInt -> IO CInt
-- SDsetaccesstype {- this function does not seem to do any useful work as in HDF-4.2.14 -}

newtype SDId = SDId Int32 deriving Eq
newtype SDataSetId (n :: Nat) (t :: HDataType a) = SDataSetId Int32 deriving Eq
newtype SDataSetRef = SDataSetRef Int32 deriving Eq
newtype SDimensionId = SDimensionId Int32 deriving Eq

data SomeSDS where
    SomeSDS :: forall (n :: Nat) (t :: HDataType a). KnownNat n =>
        HDataType a -> SDataSetId n t -> SomeSDS

instance SDObjectId SDId where
    getRawObjectId (SDId sd_id) = sd_id

instance SDObjectId (SDataSetId n t) where
    getRawObjectId (SDataSetId sds_id) = sds_id

instance SDObjectId SDimensionId where
    getRawObjectId (SDimensionId dimension_id) = dimension_id

sd_start :: String -> HDFOpenMode -> IO (Int32, SDId)
sd_start fileName mode = withCString fileName $ \c_fileName -> do
    sd_id <- c_sdstart c_fileName (toHDFOpenModeTag mode)
    return $! (sd_id, SDId sd_id)

sd_create :: forall (t :: HDataType a) (n :: Nat). KnownNat n =>
    SDId -> String -> HDataType a -> Index n -> IO (Int32, SDataSetId n t)
sd_create (SDId sd_id) sds_name data_type dim_sizes =
    withCString sds_name $ \c_sds_name ->
    withIndex dim_sizes $ \c_dim_sizes -> do
        sds_id <- c_sdcreate sd_id c_sds_name (fromHDataType data_type) rank c_dim_sizes
        return $! (sds_id, SDataSetId sds_id)
  where
    rank = fromIntegral $! natVal (Proxy :: Proxy n)

sd_select :: SDId -> Int32 -> IO (Int32, SomeSDS)
sd_select (SDId sd_id) sds_index = do
    sds_id <- c_sdselect sd_id sds_index
    (h_result, SDataSetInfoRaw{
        sDataSetRank=sdsRank
      , sDataSetDataType=HDFValue{hValueType=t}}) <- sd_getinfo (SDataSetId sds_id)
    case someNatVal (fromIntegral sdsRank) of
        SomeNat (_ :: Proxy n) -> return $! (
            if h_result == (-1) then h_result else sds_id,
            SomeSDS t (SDataSetId sds_id :: SDataSetId n t))

sd_endaccess :: SDataSetId n t -> IO (Int32, ())
sd_endaccess (SDataSetId sds_id) = do
    h_result <- c_sdendaccess sds_id
    return $! (fromIntegral h_result, ())

sd_end :: SDId -> IO (Int32, ())
sd_end (SDId sd_id) = do
    h_result <- c_sdend sd_id
    return $! (fromIntegral h_result, ())

sd_checkempty :: SDataSetId n t -> IO (Int32, Bool)
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
        else allocaArray0 (fromIntegral nameLen) $ \fileNamePtr -> do
            h_result <- c_sdgetfilename sd_id fileNamePtr
            fileName <- peekCString fileNamePtr
            return $! (fromIntegral h_result, fileName)

data SDataSetInfoRaw = SDataSetInfoRaw {
      sDataSetName     :: String
    , sDataSetRank     :: Int32
    , sDataSetDimSizes :: [Int32]
    , sDataSetDataType :: HDFType
    , sDataSetNumAttrs :: Int32
} deriving (Show, Eq)

sd_getinfo :: SDataSetId n t -> IO (Int32, SDataSetInfoRaw)
sd_getinfo sDataSetId@(SDataSetId sds_id) = do
    (h_result_getnamelen, nameLen) <- sd_getnamelen sDataSetId
    if h_result_getnamelen == (-1)
        then return (-1, emptySDataSetInfo)
        else
            alloca $ \rankPtr ->
            alloca $ \dataTypePtr ->
            alloca $ \numAttributesPtr ->
            allocaArray0 (fromIntegral nameLen) $ \sdsNamePtr ->
            allocaArray  (fromIntegral hdfMaxVarDims) $ \dimSizesPtr -> do
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
                        (fromHDFTypeTag dataType)
                        numAttributes)
  where
    emptySDataSetInfo :: SDataSetInfoRaw
    emptySDataSetInfo = SDataSetInfoRaw "" 0 [] (HDFValue HNone ()) 0

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
    withCString sds_name $ \cSDSName ->
    alloca $ \numVarsPtr -> do
        h_result <- c_sdgetnumvars_byname sd_id cSDSName numVarsPtr
        if h_result == (-1)
            then return (-1, 0)
            else do
                numVars <- peek numVarsPtr
                return $! (fromIntegral h_result, fromIntegral numVars)

sd_idtoref :: SDataSetId n t -> IO (Int32, SDataSetRef)
sd_idtoref (SDataSetId sds_id) = do
    sds_ref <- c_sdidtoref sds_id
    return $! (sds_ref, SDataSetRef sds_ref)

sd_iscoordvar :: SDataSetId n t -> IO (Int32, Bool)
sd_iscoordvar (SDataSetId sds_id) = do
    is_coordvar <- c_sdiscoordvar sds_id
    return $! (fromIntegral is_coordvar, is_coordvar /= 0)

sd_isrecord :: SDataSetId n t -> IO (Int32, Bool)
sd_isrecord (SDataSetId sds_id) = do
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

sd_getdimid :: SDataSetId n t -> Int32 -> IO (Int32, SDimensionId)
sd_getdimid (SDataSetId sds_id) dim_index = do
    dimension_id <- c_sdgetdimid sds_id (fromIntegral dim_index)
    return $! (dimension_id, SDimensionId dimension_id)

data SDimensionInfoRaw = SDimensionInfoRaw {
      sDimensionName     :: String
    , sDimensionSize     :: Int32
    , sDimensionDataType :: HDFType
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
            allocaArray0 (fromIntegral nameLen) $ \dimNamePtr -> do
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
                        (fromHDFTypeTag dataType)
                        numAttributes)
  where
    emptySDimensionInfo :: SDimensionInfoRaw
    emptySDimensionInfo = SDimensionInfoRaw "" 0 (HDFValue HNone ()) 0

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
    , sAttributeDataType :: HDFType
} deriving (Show, Eq)

sd_attrinfo :: SDObjectId id => id -> Int32 -> IO (Int32, SAttributeInfoRaw)
sd_attrinfo objId attrId =
    alloca $ \attrNValuesPtr ->
    alloca $ \dataTypePtr ->
    allocaArray0 (fromIntegral hdfMaxNcNameLen) $ \attrNamePtr -> do
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
                        (fromHDFTypeTag dataType))
  where
    emptySAttributeInfo :: SAttributeInfoRaw
    emptySAttributeInfo = SAttributeInfoRaw "" 0 (HDFValue HNone ())

data SCalibrationParametersRaw = SCalibrationParametersRaw {
      sCalibrationFactor       :: Double
    , sCalibrationScalingError :: Double
    , sUncalibratedOffset      :: Double
    , sCalibrationOffsetError  :: Double
    , sUncalibratedDataType    :: HDFType
} deriving (Show, Eq)

sd_getcal :: SDataSetId n t -> IO (Int32, SCalibrationParametersRaw)
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
                        (fromHDFTypeTag uncalibratedDataType))
  where
    emptySCalibrationParameters :: SCalibrationParametersRaw
    emptySCalibrationParameters = SCalibrationParametersRaw 1.0 0.0 0.0 0.0 (HDFValue HNone ())

data SDsetDescStringsRaw = SDsetDescStringsRaw {
      sDSLabel            :: String
    , sDSUnit             :: String
    , sDSFormat           :: String
    , sDSCoordinateSystem :: String
} deriving (Show, Eq)

sd_getdatastrs :: SDataSetId n t -> IO (Int32, SDsetDescStringsRaw)
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

sd_getfillvalue :: forall (t :: HDataType a) (n :: Nat). Storable a => SDataSetId n t -> IO (Int32, a)
sd_getfillvalue (SDataSetId sds_id) =
    alloca $ \fillValuePtr -> do
        h_result <- c_sdgetfillvalue sds_id (castPtr fillValuePtr)
        fillValue <- peek fillValuePtr
        return $! (fromIntegral h_result, fillValue)

sd_getrange :: forall (t :: HDataType a) (n :: Nat). Storable a => SDataSetId n t -> IO (Int32, (a, a))
sd_getrange (SDataSetId sds_id) =
    alloca $ \minValuePtr ->
    alloca $ \maxValuePtr -> do
        h_result <- c_sdgetrange sds_id (castPtr maxValuePtr) (castPtr minValuePtr)
        minValue <- peek minValuePtr
        maxValue <- peek maxValuePtr
        return $! (fromIntegral h_result, (minValue, maxValue))

sd_setcal :: SDataSetId n t -> SCalibrationParametersRaw -> IO (Int32, ())
sd_setcal (SDataSetId sds_id) (SCalibrationParametersRaw s sE o oE dtype) = do
    h_result <- c_sdsetcal sds_id s sE o oE (toHDFTypeTag dtype)
    return $! (fromIntegral h_result, ())

withCStringOrNull :: String -> (CString -> IO a) -> IO a
withCStringOrNull []  fun = fun nullPtr
withCStringOrNull str fun = withCString str fun

sd_setdatastrs :: SDataSetId n t -> SDsetDescStringsRaw -> IO (Int32, ())
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

sd_setfillvalue :: forall (t :: HDataType a) (n :: Nat). Storable a => SDataSetId n t -> a -> IO (Int32, ())
sd_setfillvalue (SDataSetId sds_id) fillValue =
    with fillValue $ \fillValuePtr -> do
        h_result <- c_sdsetfillvalue sds_id (castPtr fillValuePtr)
        return $! (fromIntegral h_result, ())

sd_setfillmode :: SDId -> HDFFillMode -> IO (Int32, ())
sd_setfillmode (SDId sd_id) fillMode = do
    h_result <- c_sdsetfillmode sd_id (fromIntegral . toHDFFillModeTag $ fillMode)
    return $! (fromIntegral h_result, ())

sd_setrange :: forall (t :: HDataType a) (n :: Nat). Storable a => SDataSetId n t -> a -> a -> IO (Int32, ())
sd_setrange (SDataSetId sds_id) minValue maxValue =
    with minValue $ \minValuePtr ->
    with maxValue $ \maxValuePtr -> do
        h_result <- c_sdsetrange sds_id (castPtr maxValuePtr) (castPtr minValuePtr)
        return $! (fromIntegral h_result, ())

sd_setcompress :: SDataSetId n t -> HDFCompParams -> IO (Int32, ())
sd_setcompress (SDataSetId sds_id) compParams =
    with compParams $ \compParamsPtr -> do
        h_result <- c_sdsetcompress
                        sds_id
                        (toHDFCompModeTag compParams)
                        compParamsPtr
        return $! (fromIntegral h_result, ())

sd_getcompinfo :: SDataSetId n t -> IO (Int32, HDFCompParams)
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

sd_setnbitdataset :: SDataSetId n t -> SDNBitCompParams -> IO (Int32, ())
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

sd_setchunkcache :: SDataSetId n t -> Int32 -> IO (Int32, ())
sd_setchunkcache (SDataSetId sds_id) cacheSize = do
    h_result <- c_sdsetchunkcache sds_id cacheSize 0
    return $! (fromIntegral h_result, ())

sd_setchunk :: SDataSetId n t -> HDFChunkParams -> IO (Int32, ())
sd_setchunk (SDataSetId sds_id) chunkParams =
    with chunkParams $ \chunkParamsPtr -> do
        h_result <- c_sdsetchunk sds_id chunkParamsPtr (selectChunkingMode chunkParams)
        return $! (fromIntegral h_result, ())

sd_getchunkinfo :: SDataSetId n t -> IO (Int32, HDFChunkParams)
sd_getchunkinfo (SDataSetId sds_id) =
    alloca $ \chunkModePtr ->
    alloca $ \chunkParamsPtr -> do
        -- Clear memory for HDFChunkParams because SDgetchunkinfo would set
        -- chunk_lengths according to the number of dimension in current SDS
        -- and leave the rest of the chunk_lengths array untouched.
        fillBytes chunkParamsPtr 0 $ sizeOf (undefined :: HDFChunkParams)
        h_result <- c_sdgetchunkinfo sds_id chunkParamsPtr chunkModePtr
        (fromIntegral <$> peek chunkModePtr) >>= embedCompTag chunkParamsPtr
        chunkParams <- peek chunkParamsPtr
        return $! (fromIntegral h_result, chunkParams)

type RawDataInfo = (Int32, Int32)

sd_getanndatainfo :: SDObjectId id =>
    id -> HDFAnnotationType -> IO (Int32, [RawDataInfo])
sd_getanndatainfo objId annType =
    allocaArray 128 $ \offsetArrayPtr ->
    allocaArray 128 $ \lengthArrayPtr -> do
        h_result <- c_sdgetanndatainfo
                        (getRawObjectId objId)
                        (toHDFAnnotationTypeTag annType)
                        128
                        offsetArrayPtr
                        lengthArrayPtr
        if h_result /= (-1) then do
            offsets <- peekArray (fromIntegral h_result) offsetArrayPtr
            lens <- peekArray (fromIntegral h_result) lengthArrayPtr
            return $! (fromIntegral h_result, zip offsets lens)
        else return $! (fromIntegral h_result, [])

sd_getattdatainfo :: SDObjectId id => id -> Int32 -> IO (Int32, RawDataInfo)
sd_getattdatainfo objId attrId =
    alloca $ \offsetPtr ->
    alloca $ \lengthPtr -> do
        h_result <-c_sdgetattdatainfo (getRawObjectId objId) attrId offsetPtr lengthPtr
        if h_result /= (-1) then do
            offset <- peek offsetPtr
            len <- peek lengthPtr
            return $! (fromIntegral h_result, (offset, len))
        else return $! (fromIntegral h_result, (0, 0))

sd_getoldattdatainfo :: SDataSetId n t -> Maybe SDimensionId -> String -> IO (Int32, RawDataInfo)
sd_getoldattdatainfo (SDataSetId sds_id) dimId attrName =
    withCString attrName $ \c_attrName ->
    alloca $ \offsetPtr ->
    alloca $ \lengthPtr -> do
        h_result <- c_sdgetoldattdatainfo
                        (fromMaybe 0 $ getRawObjectId <$> dimId)
                        sds_id
                        c_attrName
                        offsetPtr
                        lengthPtr
        if h_result /= (-1) then do
            offset <- peek offsetPtr
            len <- peek lengthPtr
            return $! (fromIntegral h_result, (offset, len))
        else return $! (fromIntegral h_result, (0, 0))

sd_getdatainfo :: SDataSetId n t -> [Int32] -> Int32 -> IO (Int32, [RawDataInfo])
sd_getdatainfo (SDataSetId sds_id) chunkCoords startBlock = do
    -- First, get the total number of blocks by calling SDgetdatainfo with dummy
    -- arguments, and then allocate required memory and call SDgetdatainfo for the
    -- second time to get offsets and lengths of the SDS blocks.
    num_blocks <- fromIntegral <$> c_sdgetdatainfo sds_id nullPtr 0 0 nullPtr nullPtr
    if num_blocks == (-1)
        then return $! (-1, [])
        else
            withArrayOrNull chunkCoords $ \chunkCoordsPtr ->
            allocaArray num_blocks $ \offsetArrayPtr ->
            allocaArray num_blocks $ \lengthArrayPtr -> do
                h_result <- c_sdgetdatainfo
                                sds_id
                                chunkCoordsPtr
                                (fromIntegral startBlock)
                                (fromIntegral num_blocks)
                                offsetArrayPtr
                                lengthArrayPtr
                if h_result /= (-1) then do
                    offsets <- peekArray (fromIntegral h_result) offsetArrayPtr
                    lengths <- peekArray (fromIntegral h_result) lengthArrayPtr
                    return $! (fromIntegral h_result, zip offsets lengths)
                else return $! (-1, [])

withArrayOrNull :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withArrayOrNull []   fun = fun nullPtr
withArrayOrNull vals fun = withArray vals fun

sd_getexternalinfo :: SDataSetId n t -> IO (Int32, (String, RawDataInfo))
sd_getexternalinfo (SDataSetId sds_id) = do
    name_length <- fromIntegral <$> c_sdgetexternalinfo sds_id 0 nullPtr nullPtr nullPtr
    if name_length == (-1) || name_length == 0
        then return $! (fromIntegral name_length, ("", (0, 0)))
        else
            alloca $ \offsetPtr ->
            alloca $ \lengthPtr ->
            allocaArray0 name_length $ \fileNamePtr -> do
                -- HDF library does not put zero terminator to the returned string,
                -- so we put it before calling the C function to avoid problems.
                poke (advancePtr fileNamePtr name_length) 0
                h_result <- c_sdgetexternalinfo
                                sds_id
                                (fromIntegral name_length)
                                fileNamePtr
                                offsetPtr
                                lengthPtr
                if h_result /= (-1) then do
                    offset <- peek offsetPtr
                    len <- peek lengthPtr
                    fileName <- peekCString fileNamePtr
                    return $! (fromIntegral h_result, (fileName, (offset, len)))
                else return $! (-1, ("", (0, 0)))

sd_setblocksize :: SDataSetId n t -> Int32 -> IO (Int32, ())
sd_setblocksize (SDataSetId sds_id) blockSize = do
    h_result <- c_sdsetblocksize sds_id blockSize
    return $! (fromIntegral h_result, ())

sd_setexternalfile :: SDataSetId n t -> String -> Int32 -> IO (Int32, ())
sd_setexternalfile (SDataSetId sds_id) fileName offset =
    withCString fileName $ \fileNamePtr -> do
        h_result <- c_sdsetexternalfile sds_id fileNamePtr offset
        return $! (fromIntegral h_result, ())

sd_isdimval_bwcomp :: SDimensionId -> IO (Int32, Bool)
sd_isdimval_bwcomp (SDimensionId dimension_id) = do
    h_result <- c_sdisdimval_bwcomp dimension_id
    return $! (fromIntegral h_result, toBool h_result)

sd_setdimval_comp :: SDimensionId -> Bool -> IO (Int32, ())
sd_setdimval_comp (SDimensionId dimension_id) bwCompatible = do
    h_result <- c_sdsetdimval_comp dimension_id (fromBool bwCompatible)
    return $! (fromIntegral h_result, ())

sd_setdimscale :: Storable a =>
    SDimensionId -> HDataType a -> VS.Vector a -> IO (Int32, ())
sd_setdimscale (SDimensionId dimension_id) data_type dim_scale =
    VS.unsafeWith dim_scale $ \dimScalePtr -> do
        h_result <- c_sdsetdimscale
                        dimension_id
                        (fromIntegral $ VS.length dim_scale)
                        (fromHDataType data_type)
                        (castPtr dimScalePtr)
        return $! (fromIntegral h_result, ())

sd_getdimscale :: SDataSetId n t -> SDimensionId -> IO (Int32, HDFVector)
sd_getdimscale sds sDimensionId@(SDimensionId dimension_id) = do
    (h_result_1, SDimensionInfoRaw{
          sDimensionSize=s_dim
        , sDimensionDataType=HDFValue{hValueType=t :: HDataType dt}}) <- sd_diminfo sDimensionId
    -- Unlimited dimension size is reported as 0 by sd_diminfo, so it should be retrieved
    -- from the sd_getinfo output.
    (h_result_2, s) <- if s_dim == 0 && h_result_1 /= (-1)
        then second (head . sDataSetDimSizes) <$> sd_getinfo sds
        else return (h_result_1, s_dim)
    if h_result_2 == (-1)
        then return (h_result_2, HDFValue HNone VS.empty)
        else case t of
            HNone -> return (h_result_2, HDFValue HNone VS.empty)
            _ -> do
                (fp :: ForeignPtr dt) <- mallocForeignPtrArray $ fromIntegral s
                h_result_3 <- withForeignPtr fp $ \dimScalePtr -> do
                    c_sdgetdimscale dimension_id (castPtr dimScalePtr)
                return $!
                    ( fromIntegral h_result_3
                    , HDFValue t $ VS.unsafeFromForeignPtr0 fp (fromIntegral s))

sd_setattr :: (SDObjectId id, Storable a, CanBeAttribute t, a `IsElementOf` t) =>
    id -> String -> HDataType a -> t -> IO (Int32, ())
sd_setattr objId attrName data_type attrValue =
    withCString attrName $ \attrNamePtr ->
    withAttributePtr attrValue $ \attrValuePtr -> do
        h_result <- c_sdsetattr
                        (getRawObjectId objId)
                        attrNamePtr
                        (fromHDataType data_type)
                        (fromIntegral $ attributeLen attrValue)
                        attrValuePtr
        return $! (fromIntegral h_result, ())

sd_readattr :: SDObjectId id => id -> Int32 -> IO (Int32, HDFVector)
sd_readattr objId attrId = do
    (h_result_1, SAttributeInfoRaw{
          sAttributeNValues=attrNValues
        , sAttributeDataType=HDFValue{hValueType=t :: HDataType dt}}) <- sd_attrinfo objId attrId
    if h_result_1 == (-1)
        then return (h_result_1, HDFValue HNone VS.empty)
        else case t of
            HNone -> return (h_result_1, HDFValue HNone VS.empty)
            _ -> do
                (fp :: ForeignPtr dt) <- mallocForeignPtrArray $ fromIntegral attrNValues
                h_result_2 <- withForeignPtr fp $ \dimScalePtr -> do
                    c_sdreadattr (getRawObjectId objId) attrId (castPtr dimScalePtr)
                return $!
                    ( fromIntegral h_result_2
                    , HDFValue t $ VS.unsafeFromForeignPtr0 fp (fromIntegral attrNValues))

sd_writechunk :: forall (t :: HDataType a) (n :: Nat). Storable a =>
    SDataSetId n t -> [Int32] -> VS.Vector a -> IO (Int32, ())
sd_writechunk (SDataSetId sds_id) chunkCoords dataChunk =
    withArray chunkCoords $ \chunkCoordsPtr ->
    VS.unsafeWith dataChunk $ \dataChunkPtr -> do
        h_result <- c_sdwritechunk
                        sds_id
                        chunkCoordsPtr
                        (castPtr dataChunkPtr)
        return $! (fromIntegral h_result, ())

sd_readchunk :: forall (t :: HDataType a) (n :: Nat). Storable a =>
    SDataSetId n t -> [Int32] -> IO (Int32, VS.Vector a)
sd_readchunk sds@(SDataSetId sds_id) chunkCoords = do
    (h_result, chunkLen) <- second (product . hdfChunkSizes) <$> sd_getchunkinfo sds
    if h_result == (-1)
        then return (h_result, VS.empty)
        else withArray chunkCoords $ \chunkCoordsPtr -> do
            fp <- mallocForeignPtrArray $ fromIntegral chunkLen
            h_result_2 <- withForeignPtr fp $ \dataChunkPtr ->
                c_sdreadchunk sds_id chunkCoordsPtr (castPtr dataChunkPtr)
            return $!
                ( fromIntegral h_result_2
                , VS.unsafeFromForeignPtr0 fp (fromIntegral chunkLen))

sd_readdata :: forall (t :: HDataType a) (n :: Nat). (Storable a, KnownNat n) =>
    SDataSetId n t -> Index n -> Index n -> Index n -> IO (Int32, VS.Vector a)
sd_readdata (SDataSetId sds_id) start stride edges =
    withIndex start $ \startPtr ->
    withIndex stride $ \stridePtr ->
    withIndex edges $ \edgesPtr -> do
        edgesList <- peekArray (fromIntegral $! natVal (Proxy :: Proxy n)) edgesPtr
        fp <- mallocForeignPtrArray . fromIntegral $ product edgesList
        h_result <- withForeignPtr fp $ \sdsDataPtr ->
            c_sdreaddata sds_id startPtr stridePtr edgesPtr (castPtr sdsDataPtr)
        return $!
            ( fromIntegral h_result
            , VS.unsafeFromForeignPtr0 fp (fromIntegral $ product edgesList))

sd_writedata :: forall (t :: HDataType a) (n :: Nat). (Storable a, KnownNat n) =>
    SDataSetId n t -> Index n -> Index n -> Index n -> VS.Vector a -> IO (Int32, ())
sd_writedata (SDataSetId sds_id) start stride edges sdsData =
    withIndex start $ \startPtr ->
    withIndex stride $ \stridePtr ->
    withIndex edges $ \edgesPtr ->
    VS.unsafeWith sdsData $ \sdsDataPtr -> do
        h_result <- c_sdwritedata sds_id startPtr stridePtr edgesPtr (castPtr sdsDataPtr)
        return $! (fromIntegral h_result, ())
