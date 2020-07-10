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
import           GHC.Stack

import           Data.Format.HDF.LowLevel.C.Definitions
import           Data.Format.HDF.LowLevel.Definitions
import           Data.Format.HDF.LowLevel.Definitions.Internal
import           Data.Format.HDF.LowLevel.HE

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

{-# NOINLINE currentLine #-}
currentLine :: HasCallStack => Int
currentLine = srcLocStartLine . snd . head . getCallStack $ callStack

{- | Handle for HDF file opened with SD interface. -}
newtype SDId = SDId Int32 deriving Eq

{- | Object id for a scientific dataset (SDS) within an HDF file.

The 'SDataSetId' type also contains information about the dataset rank and data
type of the SDS values.
-}
newtype SDataSetId (n :: Nat) (t :: HDataType a) = SDataSetId Int32 deriving Eq

{- | Reference to an SDS object.

Reference can be used to add the data set to a vgroup as well as a means of using
the HDF annotations interface to annotate the data set.
-}
newtype SDataSetRef = SDataSetRef Int32 deriving Eq

{- | Object id for a dimension associated to an SDS -}
newtype SDimensionId = SDimensionId Int32 deriving Eq

{- | SDS object id with non-static rank and type.

'SomeSDS' is returned when 'sd_select' is called to get SDS object id
from an HDF file because SDS rank and data type of stored values are not known
at compile time. When calling SDS function which requires information
about rank and type, e.g., 'sd_readdata', pattern-match on 'SomeSDS'
'HDataType' field and 'SDataSetId' rank.
-}
data SomeSDS where
    SomeSDS :: forall (n :: Nat) (t :: HDataType a). KnownNat n =>
        HDataType a -> SDataSetId n t -> SomeSDS

instance SDObjectId SDId where
    getRawObjectId (SDId sd_id) = sd_id

instance SDObjectId (SDataSetId n t) where
    getRawObjectId (SDataSetId sds_id) = sds_id

instance SDObjectId SDimensionId where
    getRawObjectId (SDimensionId dimension_id) = dimension_id

{-| Opens an HDF file and initializes an SD interface.

'sd_start' opens the file with the name specified by the first parameter, with
the access mode specified by the second parameter, and returns an SD
interface identifier ('SDId'). This routine must be called for each file before any
other SD calls can be made on that file.

The type of identifier returned by 'sd_start' is currently not the same as the
identifier returned by 'Data.Format.HDF.LowLevel.hdf_open'. As a result, the SD
interface identifiers ('SDId') returned by this routine are not understood by other
HDF interfaces. To mix SD API calls and other HDF API calls, use 'sd_start' and
'hdf_open' on the same file. 'sd_start' must precede all SD calls, and 'hdf_open'
must precede all other HDF function calls. To terminate access to the file, use
'sd_end' to dispose of the SD interface identifier, and 'Data.Format.HDF.LowLevel.hdf_close'
to dispose of the file identifier.

The file identified by the first parameter can be any one of the following:
an "old-style" DFSD file or a "new-style" SD file.
-}
sd_start :: String -> HDFOpenMode -> IO (Int32, SDId)
sd_start fileName mode = withCString fileName $ \c_fileName -> do
    sd_id <- c_sdstart c_fileName (toHDFOpenModeTag mode)
    return $! (sd_id, SDId sd_id)

{-| Creates a new data set.

'sd_create' creates a data set with the name, number type, number of
dimensions, dimension sizes specified by the parameters.

Once a data set has been created, it is not possible to change its name, data
type, or rank. However, it is possible to create a data set and close the file
before writing any data values to it. The values can be added or modified at a
future time. To add data or modify an existing data set, use 'sd_select' to get the
data set identifier instead of 'sd_create'.
If the parameter name an empty string, the default name "DataSet" will be generated.
The length of the name specified by the name parameter is no longer limited to 64
characters starting in HDF 4.2r2.
Note that when an older version of the library reads a data set, which was
created by a library of version 4.2r2 or later and has the name that is longer
than 64 characters, the retrieved name will contain some garbage after 64
characters.
The calling program must ensure that the length of the dimension sizes parameter is
between 0 and 32.
Note that, in order for HDF4 and NetCDF models to work together, HDF
allows SDS to have rank 0 . However, there is no intention for data to be
written to this type of SDS, but only to store attribute as part of the data
description. Consequently, setting compression and setting chunk are
disallowed.

To create a data set with an unlimited dimension, set the first dimension
to be of zero length.

See the notes regarding the potential performance impact of unlimited
dimension data sets in Section 14.4.3, "Unlimited Dimension Data Sets (SDSs
and Vdatas) and Performance" the HDF User's Guide.
-}
sd_create :: forall (t :: HDataType a) (n :: Nat). KnownNat n =>
    SDId
  -> String -- ^ Name of the new SDS
  -> HDataType a -- ^ Data type of the values held by the new SDS
  -> Index n -- ^ Rank of the new SDS
  -> IO (Int32, SDataSetId n t)
sd_create (SDId sd_id) sds_name data_type dim_sizes =
    withCString sds_name $ \c_sds_name ->
    withIndex dim_sizes $ \c_dim_sizes -> do
        sds_id <- c_sdcreate sd_id c_sds_name (fromHDataType data_type) rank c_dim_sizes
        return $! (sds_id, SDataSetId sds_id)
  where
    rank = fromIntegral $! natVal (Proxy :: Proxy n)

{-| Obtains the data set identifier of a data set.

SDselect obtains the data set identifier of the data set specified by its
index.

The integration with netCDF has required that a dimension (or coordinate
variable) is stored as a data set in the file. Therefore, the value of index
may correspond to the coordinate variable instead of the actual data set. Users
should use the routine 'sd_iscoordvar' to determine whether the given data set is
a coordinate variable.

The value of index is greater than or equal to 0 and less than the number of
data sets in the file. The total number of data sets in a file may be obtained
from a call to 'sd_fileinfo'. The 'sd_nametoindex' routine can be used to find the
index of a data set if its name is known. However, when multiple data sets
have the same name, 'sd_nametoindices' can be used to obtain all the indices.
-}
sd_select ::
    SDId
  -> Int32 -- ^ Dataset index
  -> IO (Int32, SomeSDS)
sd_select (SDId sd_id) sds_index = do
    sds_id <- c_sdselect sd_id sds_index
    (h_result, SDataSetInfoRaw{
        sDataSetRank=sdsRank
      , sDataSetDataType=HDFValue{hValueType=t}}) <- sd_getinfo (SDataSetId sds_id)
    case someNatVal (fromIntegral sdsRank) of
        SomeNat (_ :: Proxy n) -> return $! (
            if h_result == (-1) then h_result else sds_id,
            SomeSDS t (SDataSetId sds_id :: SDataSetId n t))

{-| Terminates access to a data set.

'sd_endaccess' frees the memory taken up by the HDF library’s data structures
devoted to the data set identified by the argument.

Failing to call this routine after all operations on the specified data set are
complete may result in loss of data. This routine must be called once for each
call to 'sd_create' or 'sd_select'.
-}
sd_endaccess :: SDataSetId n t -> IO (Int32, ())
sd_endaccess (SDataSetId sds_id) = do
    h_result <- c_sdendaccess sds_id
    return $! (fromIntegral h_result, ())

{-| Terminates access to an SD interface.

'sd_end' closes the file and frees memory allocated by the library when SD
interface activities are completed. If the calling program exits without invoking
this routine, recent changes made to the in-core file data are likely not to be
flushed to the file. Note that each 'sd_start' must have a matching 'sd_end'.
-}
sd_end :: SDId -> IO (Int32, ())
sd_end (SDId sd_id) = do
    h_result <- c_sdend sd_id
    return $! (fromIntegral h_result, ())

{-| Determines whether an SDS is empty.

'sd_checkempty' checks if SDS in question has not been written with data.
It returns 'True' if SDS is empty and 'False' otherwise.
-}
sd_checkempty :: SDataSetId n t -> IO (Int32, Bool)
sd_checkempty (SDataSetId sds_id) = alloca $ \emptySDSPtr -> do
    h_result <- c_sdcheckempty sds_id emptySDSPtr
    res <- if h_result == (-1)
        then return (-1)
        else peek emptySDSPtr
    return $! (h_result, res /= 0)

{-| Retrieves the number of data sets and the number of global attributes in a file.

'sd_fileinfo' returns the number of data sets and the number of global attributes. The term
"global attributes" refers to attributes that are assigned to the file. The global
attributes are created by 'sd_setattr' using an SD interface identifier rather than
a data set identifier.

The returned number of data sets includes the number of coordinate variable data sets.
To determine if the data set is a coordinate variable use 'sd_iscoordvar'.
-}
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

{-| Retrieves the length of the name of a file, a dataset, or a dimension.

Given an identifier of a file, a dataset, or a dimension, 'sd_getnamelen' retrieves
the length of its name.
-}
sd_getnamelen :: SDObjectId id => id -> IO (Int32, Int32)
sd_getnamelen objId = alloca $ \nameLenPtr -> do
    h_result <- c_sdgetnamelen (getRawObjectId objId) nameLenPtr
    res <- if h_result == (-1)
        then return 0
        else peek nameLenPtr
    return $! (fromIntegral h_result, fromIntegral res)

{-| Given a file identifier, retrieves the name of the file. -}
sd_getfilename :: SDId -> IO (Int32, String)
sd_getfilename sdFileId@(SDId sd_id) = do
    (h_result_getnamelen, nameLen) <- sd_getnamelen sdFileId
    if h_result_getnamelen == (-1)
        then return (-1, "")
        else allocaArray0 (fromIntegral nameLen) $ \fileNamePtr -> do
            h_result <- c_sdgetfilename sd_id fileNamePtr
            fileName <- peekCString fileNamePtr
            return $! (fromIntegral h_result, fileName)

{- | General information about SDS -}
data SDataSetInfoRaw = SDataSetInfoRaw {
      sDataSetName     :: String
    , sDataSetRank     :: Int32
    , sDataSetDimSizes :: [Int32]
    , sDataSetDataType :: HDFType
    , sDataSetNumAttrs :: Int32
} deriving (Show, Eq)

{-| Retrieves the name, rank, dimension sizes, number type and number of
attributes for a data set.

If the data set is created with an unlimited dimension, then the first element of
the 'sDataSetDimSizes' contains the number of records in the unlimited dimension.
Use 'sd_isrecord' to determine if the data set has an unlimited dimension.
-}
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

{-| Retrieves current and maximum number of open files.

'sd_get_maxopenfiles' retrieves the current number of open files allowed in
HDF and the maximum number of open files allowed on the system.
-}
sd_get_maxopenfiles :: IO (Int32, (Int32, Int32))
sd_get_maxopenfiles =
    alloca $ \currMaxPtr ->
    alloca $ \sysLimitPtr -> do
        h_result <- c_sdget_maxopenfiles currMaxPtr sysLimitPtr
        currMax <- peek currMaxPtr
        sysLimit <- peek sysLimitPtr
        return $! (fromIntegral h_result, (fromIntegral currMax, fromIntegral sysLimit))

{-| Returns the number of files currently being opened. -}
sd_get_numopenfiles :: IO (Int32, Int32)
sd_get_numopenfiles = do
    h_result <- c_sdget_numopenfiles
    return $! (fromIntegral h_result, fromIntegral h_result)

{-| Get the number of data sets having the same name.

'sd_getnumvars_byname' retrieves the number of variables with the specified name
specified. The variables may include both data sets or coordinate variables.
The routine does not accept wildcards in the specified data set name. It also
searches on that name in a case-sensitive manner.
-}
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

{-| Returns the reference assigned to a data set.

'sd_idtoref' returns the reference of the data set. This reference is assigned
by the HDF library when the data set is created. The specified reference can be
used to add the data set to a vgroup as well as a means of using the HDF annotations
interface to annotate the data set.
-}
sd_idtoref :: SDataSetId n t -> IO (Int32, SDataSetRef)
sd_idtoref (SDataSetId sds_id) = do
    sds_ref <- c_sdidtoref sds_id
    return $! (sds_ref, SDataSetRef sds_ref)

{-| Determines if a data set is a coordinate variable.

Returns 'True' if the data set is a coordinate variable, and 'False'
otherwise.
-}
sd_iscoordvar :: SDataSetId n t -> IO (Int32, Bool)
sd_iscoordvar (SDataSetId sds_id) = do
    is_coordvar <- c_sdiscoordvar sds_id
    return $! (fromIntegral is_coordvar, is_coordvar /= 0)

{-| Determines whether a data set is appendable.

Returns 'True' if the data set is appendable, and 'False' otherwise.

'sd_isrecord' will determine if the data set is appendable, which means
that the slowest-changing dimension was declared unlimited when the data set was created.
-}
sd_isrecord :: SDataSetId n t -> IO (Int32, Bool)
sd_isrecord (SDataSetId sds_id) = do
    is_record <- c_sdisrecord sds_id
    return $! (fromIntegral is_record, is_record /= 0)

{- | Determines the index of a data set given its name.

Returns the index of the data set if the data set is found.

'sd_nametoindex' returns the index of the data set with the name specified by
the argument. The routine does not accept wildcards in the specified
data set name. It also searches on that name in a case-sensitive manner. If
there are more than one data set with the same name, the routine will return the
index of the first one.

Note that if there are more than one data set with the same name in the file,
writing to a data set returned by this function without verifying that it is the
desired data set could cause data corruption.

'sd_getnumvars_byname' can be used to get the number of data sets (or
variables, which includes both data sets and coordinate variables) with the
same name. 'sd_nametoindices' can be used to get a list of structures
containing the indices and the types of all the variables of that same name.
-}
sd_nametoindex :: SDId -> String -> IO (Int32, Int32)
sd_nametoindex (SDId sd_id) sds_name = withCString sds_name $ \cSDSName -> do
    sds_index <- c_sdnametoindex sd_id cSDSName
    if sds_index == (-1)
      then he_push DFE_SDS_NOTFOUND "sd_nametoindex" "Data.Format.HDF.LowLevel.SD" currentLine
      else return ()
    return $! (sds_index, sds_index)

{- | Retrieves indices of all variables with the same name.

'sd_nametoindices' retrieves a list of structures 'HDFVarList' , containing the
indices and the types of all variables of the same name.

The routine does not accept wildcards in the specified data set name. It also
searches on that name in a case-sensitive manner.
-}
sd_nametoindices :: SDId -> String -> IO (Int32, [HDFVarList])
sd_nametoindices sd@(SDId sd_id) sds_name = do
    (h_result_getnumvars_byname, numVars) <- sd_getnumvars_byname sd sds_name
    if h_result_getnumvars_byname == (-1)
        then do
          he_push DFE_SDS_NOTFOUND "sd_nametoindices" "Data.Format.HDF.LowLevel.SD" currentLine
          return $! (h_result_getnumvars_byname, [])
        else
            withCString sds_name $ \cSDSName ->
            allocaArray (fromIntegral numVars) $ \sdsVarListPtr -> do
                h_result <- c_sdnametoindices sd_id cSDSName sdsVarListPtr
                sdsVarList <- peekArray (fromIntegral numVars) sdsVarListPtr
                return $! (fromIntegral h_result, sdsVarList)

{- | Returns the index of a data set given its reference.

Returns the index of the data set if the data set is found.

The value returned by 'sd_reftoindex' can be passed to 'sd_select' to
obtain a data set identifier.
-}
sd_reftoindex :: SDId -> SDataSetRef -> IO (Int32, Int32)
sd_reftoindex (SDId sd_id) (SDataSetRef sds_ref) = do
    sds_index <- c_sdreftoindex sd_id sds_ref
    return $! (sds_index, sds_index)

{- | Resets the maximum number of files can be opened at the same time.

Returns the current maximum number of opened files allowed if successful.

Prior to release 4.2r2, the maximum number of files that can be opened at the
same time was limited to 32. In HDF 4.2r2 and later versions, if this limit is
reached, the library will increase it to the system limit minus 3 to account for
stdin, stdout, and stderr.

This function can be called anytime to change the maximum number of open
files allowed in HDF. If 0 is passed, 'sd_reset_maxopenfiles' will
simply return the current maximum number of open files allowed. If the argument
exceeds system limit, 'sd_reset_maxopenfiles' will reset the maximum number
of open files to the system limit, and return that value.

Furthermore, if the system maximum limit is reached, the library will push the
error 'Data.Format.HDF.LowLevel.Definitions.DFE_TOOMANY' onto the error stack.
User applications can detect this after an 'sd_start' fails.
-}
sd_reset_maxopenfiles :: Int32 -> IO (Int32, Int32)
sd_reset_maxopenfiles newLimit = do
    current_limit <- c_sdreset_maxopenfiles (fromIntegral newLimit)
    return $! (fromIntegral current_limit, fromIntegral current_limit)

{- | Returns the identifier of a dimension given its index.

The dimension index is a non-negative integer and is less than the total number
of data set dimensions returned by 'sd_getinfo'.
-}
sd_getdimid :: SDataSetId n t -> Int32 -> IO (Int32, SDimensionId)
sd_getdimid (SDataSetId sds_id) dim_index = do
    dimension_id <- c_sdgetdimid sds_id (fromIntegral dim_index)
    return $! (dimension_id, SDimensionId dimension_id)

{- | General information about SDS dimension -}
data SDimensionInfoRaw = SDimensionInfoRaw {
      sDimensionName     :: String
    , sDimensionSize     :: Int32
    , sDimensionDataType :: HDFType
    , sDimensionNumAttrs :: Int32
} deriving (Show, Eq)

{- | Retrieves information about a dimension.

'sd_diminfo' retrieves the name, size, number type, and number of values of the
dimension.

If the output value of 'sDimensionSize' is 0 , then the dimension is unlimited.
To get the number of records of an unlimited dimension, use 'sd_getinfo'.

If scale information has been stored for this dimension via 'sd_setdimscale',
'sDimensionDataType' will contain the type specification. If no scale
information has been stored for this dimension, this field will be set to
@'Data.Format.HDF.LowLevel.Definitions.HDFValue' 'Data.Format.HDF.LowLevel.Definitions.HNone' ()@.

If the user has not named the dimension via 'sd_setdimname', a default
dimension name of "fakeDim[x]" will be generated by the library, where [x]
denotes the dimension index.
-}
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

{- | Assigns a name to a dimension.

Dimensions that are not explicitly named by the user will have the default name
of "fakeDim[x]" specified by the HDF library, where [x] denotes the dimension index.
If another dimension exists with the same name it is assumed that they refer to
the same dimension object and changes to one will be reflected in the other. If
the dimension with the same name has a different size, an error condition will
result.

The length of dimension name can be at most 64 characters.
Naming dimensions is optional but encouraged.
-}
sd_setdimname :: SDimensionId -> String -> IO (Int32, ())
sd_setdimname (SDimensionId dimension_id) dimension_name =
    withCString dimension_name $ \c_dimension_name -> do
        h_result <- c_sdsetdimname dimension_id c_dimension_name
        return $! (fromIntegral h_result, ())

{- | Finds the index of an attribute given its name.

The object id can be either an SD interface identifier, returned by
'sd_start', a data set identifier, returned by 'sd_select', or a dimension
identifier, returned by 'sd_getdimid'.

Wildcard characters are not allowed in the parameter attr_name. 'sd_findattr'
searches for the name in a case-sensitive manner.
-}
sd_findattr :: SDObjectId id => id -> String -> IO (Int32, Int32)
sd_findattr objId attribute_name =
    withCString attribute_name $ \c_attribute_name -> do
        attribute_index <- c_sdfindattr (getRawObjectId objId) c_attribute_name
        if attribute_index == (-1)
          then he_push DFE_CANTGETATTR "sd_findattr" "Data.Format.HDF.LowLevel.SD" currentLine
          else return ()
        return (attribute_index, attribute_index)

{- | General information about an attribute -}
data SAttributeInfoRaw = SAttributeInfoRaw {
      sAttributeName     :: String
    , sAttributeNValues  :: Int32
    , sAttributeDataType :: HDFType
} deriving (Show, Eq)

{- | Retrieves information about an attribute.

The object id can be either an SD interface identifier, returned by
'sd_start', a data set identifier, returned by 'sd_select', or a dimension
identifier, returned by 'sd_getdimid'.

Valid values of the attribute index range from 0 to the number of
attributes attached to the object - 1 .
-}
sd_attrinfo :: SDObjectId id =>
    id
  -> Int32 -- ^ Attribute index
  -> IO (Int32, SAttributeInfoRaw)
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

{- | Calibration parameters of a calibrated SDS

__NOTE:__ HDF library __does not__ perform packing and unpacking while reading and
writing scientific datasets. It is user responsibility to use 'sd_getcal' to
get calibration parameters and restore original values when reading SDS, and
pack fields and set correct calibration parameters with 'sd_setcal' when writing SDS.
-}
data SCalibrationParametersRaw = SCalibrationParametersRaw {
      sCalibrationFactor       :: Double
    , sCalibrationScalingError :: Double
    , sUncalibratedOffset      :: Double
    , sCalibrationOffsetError  :: Double
    , sUncalibratedDataType    :: HDFType
} deriving (Show, Eq)

{- | Retrieves the calibration information associated with a data set.

The relationship between a calibrated value @cal_value@ and the original value @orig_value@
is defined as @orig_value = 'sCalibrationFactor' * (cal_value - 'sUncalibratedOffset')@.

The 'sCalibrationOffsetError' field contains a potential error of offset , and 'sCalibrationScalingError'
contains a potential error of calibration factor. Currently the calibration record is provided
for information only. The SD interface performs no operations on the data
based on the calibration tag.
-}
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

{- | Predefined attributes for SDS.

When a 'SDsetDescStringsRaw' field is set to an empty string and 'sd_setdatastrs'
is called, corresponding attribute will not be created.
-}
data SDsetDescStringsRaw = SDsetDescStringsRaw {
      sDSLabel            :: String
    , sDSUnit             :: String
    , sDSFormat           :: String
    , sDSCoordinateSystem :: String
} deriving (Show, Eq)

{- | Retrieves the predefined attributes of a data set.

'sd_getdatastrs' retrieves the predefined attributes for a data set.
The predefined attributes are label, unit, format, and coordinate system.
They are then stored in the parameters @label@, @unit@, @format@, and @coordsys@,
respectively. Refer to Section 3.10, "Predefined Attributes" of
the HDF User's Guide for more information on predefined attributes.
-}
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

{- | Predefined attributes for an SDS dimension.

When a 'SDimDescStringsRaw' field is set to an empty string and 'sd_setdimstrs'
is called, corresponding attribute will not be created.
-}
data SDimDescStringsRaw = SDimDescStringsRaw {
      sDimLabel            :: String
    , sDimUnit             :: String
    , sDimFormat           :: String
} deriving (Show, Eq)

{- | Retrieves the predefined attributes of a dimension.

'sd_getdimstrs' retrieves the predefined attributes associated with a dimension.
The predefined attributes are label, unit, and format. These predefined attributes
are stored in the parameters @label@, @unit@, and @format@, respectively. Refer to Table 3.10,
"Predefined Attributes", in the HDF User's Guide for more information on predefined attributes.
-}
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

{- | Reads the fill value of a data set, if the value has been set.-}
sd_getfillvalue :: forall (t :: HDataType a) (n :: Nat). Storable a => SDataSetId n t -> IO (Int32, a)
sd_getfillvalue (SDataSetId sds_id) =
    alloca $ \fillValuePtr -> do
        h_result <- c_sdgetfillvalue sds_id (castPtr fillValuePtr)
        fillValue <- peek fillValuePtr
        return $! (fromIntegral h_result, fillValue)

{- | Retrieves the maximum and minimum values of the range.

The term "range" is used here to describe the range of numeric values
stored in a data set.

The maximum and minimum values must be previously set via a call to 'sd_setrange'.
-}
sd_getrange :: forall (t :: HDataType a) (n :: Nat). Storable a => SDataSetId n t -> IO (Int32, (a, a))
sd_getrange (SDataSetId sds_id) =
    alloca $ \minValuePtr ->
    alloca $ \maxValuePtr -> do
        h_result <- c_sdgetrange sds_id (castPtr maxValuePtr) (castPtr minValuePtr)
        minValue <- peek minValuePtr
        maxValue <- peek maxValuePtr
        return $! (fromIntegral h_result, (minValue, maxValue))

{- | Sets the calibration information.

See 'sd_getcal' documentation for additional information about calibrated datasets.
-}
sd_setcal :: SDataSetId n t -> SCalibrationParametersRaw -> IO (Int32, ())
sd_setcal (SDataSetId sds_id) (SCalibrationParametersRaw s sE o oE dtype) = do
    h_result <- c_sdsetcal sds_id s sE o oE (toHDFTypeTag dtype)
    return $! (fromIntegral h_result, ())

withCStringOrNull :: String -> (CString -> IO a) -> IO a
withCStringOrNull []  fun = fun nullPtr
withCStringOrNull str fun = withCString str fun

{- | Sets the predefined attributes for a data set.

See 'sd_getdatastrs' documentation for additional information about predefined attributes for a dataset.
-}
sd_setdatastrs :: SDataSetId n t -> SDsetDescStringsRaw -> IO (Int32, ())
sd_setdatastrs (SDataSetId sds_id) (SDsetDescStringsRaw l u f c) =
    withCStringOrNull l $ \lPtr ->
    withCStringOrNull u $ \uPtr ->
    withCStringOrNull f $ \fPtr ->
    withCStringOrNull c $ \cPtr -> do
        h_result <- c_sdsetdatastrs sds_id lPtr uPtr fPtr cPtr
        return $! (fromIntegral h_result, ())

{- | Sets the predefined attribute of a dimension.

See 'sd_getdimstrs' documentation for additional information about predefined attributes for a dimension.
-}
sd_setdimstrs :: SDimensionId -> SDimDescStringsRaw -> IO (Int32, ())
sd_setdimstrs (SDimensionId dimension_id) (SDimDescStringsRaw l u f) =
    withCStringOrNull l $ \lPtr ->
    withCStringOrNull u $ \uPtr ->
    withCStringOrNull f $ \fPtr -> do
        h_result <- c_sdsetdimstrs dimension_id lPtr uPtr fPtr
        return $! (fromIntegral h_result, ())

{- | Sets the fill value for a data set.

It is recommended to call 'sd_setfillvalue' before writing data.
-}
sd_setfillvalue :: forall (t :: HDataType a) (n :: Nat). Storable a => SDataSetId n t -> a -> IO (Int32, ())
sd_setfillvalue (SDataSetId sds_id) fillValue =
    with fillValue $ \fillValuePtr -> do
        h_result <- c_sdsetfillvalue sds_id (castPtr fillValuePtr)
        return $! (fromIntegral h_result, ())

{- | Sets the current fill mode of a file.

Returns the fill mode value before it was reset if successful.

'sd_setfillmode' applies the fill mode specified by the argument to all
data sets contained in the file identified by the id parameter.

Possible values of fill mode are 'Data.Format.HDF.LowLevel.Definitions.HDFFill' and
'Data.Format.HDF.LowLevel.Definitions.HDFNoFill'. 'Data.Format.HDF.LowLevel.Definitions.HDFFill' is
the default mode, and indicates that fill values will be written when
the data set is created. 'Data.Format.HDF.LowLevel.Definitions.HDFNoFill' indicates
that fill values will not be written.

When a data set without unlimited dimensions is created, by default the first
'sd_writedata' call will fill the entire data set with the default or user-defined
fill value (set by 'sd_setfillvalue'). In data sets with an unlimited dimension, if a
new write operation takes place along the unlimited dimension beyond the last
location of the previous write operation, the array locations between these
written areas will be initialized to the user-defined fill value, or the default fill
value if a user-defined fill value has not been specified.

If it is certain that all data set values will be written before any read operation
takes place, there is no need to write the fill values. Simply call 'sd_setfillmode'
with fill_mode value set to 'Data.Format.HDF.LowLevel.Definitions.HDFNoFill', which
will eliminate all fill value write operations to the data set. For large data sets,
this can improve the speed by almost 50%.
-}
sd_setfillmode :: SDId -> HDFFillMode -> IO (Int32, HDFFillMode)
sd_setfillmode (SDId sd_id) fillMode = do
    h_result <- c_sdsetfillmode sd_id (fromIntegral . toHDFFillModeTag $ fillMode)
    case fromHDFFillModeTag (fromIntegral h_result) of
        Just oldFillMode -> return $! (fromIntegral h_result, oldFillMode)
        -- When SDsetfillmode fails return the default fill mode as previous mode
        Nothing          -> return $! (fromIntegral h_result, HDFFill)

{- | Sets the maximum and minimum range values for a data set.

'sd_setrange' sets the maximum and minimum range values of a data set.
The term "range" is used here to describe the range of numeric values
stored in a data set.

This routine does not compute the maximum and minimum range values, it
only stores the values as given. As a result, the maximum and minimum range
values may not always reflect the actual maximum and minimum range values
in the data set data.
-}
sd_setrange :: forall (t :: HDataType a) (n :: Nat). Storable a =>
    SDataSetId n t
  -> a -- ^ Minimum value
  -> a -- ^ Maximum value
  -> IO (Int32, ())
sd_setrange (SDataSetId sds_id) minValue maxValue =
    with minValue $ \minValuePtr ->
    with maxValue $ \maxValuePtr -> do
        h_result <- c_sdsetrange sds_id (castPtr maxValuePtr) (castPtr minValuePtr)
        return $! (fromIntegral h_result, ())

{- | Compresses the data set with the specified compression method.

'sd_setcompress' compresses a data set according to the specified
compression method. 'sd_setcompress' sets up the special element for the
compressed data written during the next call to 'sd_writedata'.

Note that for 'Data.Format.HDF.LowLevel.C.Definitions.HDFCompNBit' 'sd_setnbitdataset'
should be used instead.
-}
sd_setcompress :: SDataSetId n t -> HDFCompParams -> IO (Int32, ())
sd_setcompress (SDataSetId sds_id) compParams =
    with compParams $ \compParamsPtr -> do
        h_result <- c_sdsetcompress
                        sds_id
                        (toHDFCompModeTag compParams)
                        compParamsPtr
        return $! (fromIntegral h_result, ())

{- | Retrieves data set compression type and compression information.

'sd_getcompinfo' will succeed and return 'Data.Format.HDF.LowLevel.C.Definitions.HDFCompNone'
if either of the following conditions exists: the data set is not compressed or no data
has been written to the SDS
-}
sd_getcompinfo :: SDataSetId n t -> IO (Int32, HDFCompParams)
sd_getcompinfo (SDataSetId sds_id) =
    alloca $ \compTypePtr ->
    alloca $ \compParamsPtr -> do
        h_result <- c_sdgetcompinfo sds_id compTypePtr compParamsPtr
        peek compTypePtr >>= embedCompTag compParamsPtr
        compParams <- peek compParamsPtr
        return $! (fromIntegral h_result, compParams)

{- | User-defined parameters of n-bit compression

Any length between 1 and 32 bits can be specified. After 'sd_setnbitdataset' has
been called for the data set array, any read or write operations will involve a
conversion between the new data length of the data set array and the data
length of the read or write buffer.

Bit lengths of all number types are counted from the right of the bit field
starting with 0. In a bit field containing the values @01111011@, bits 2 and 7 are
set to @0@ and all the other bits are set to @1@.
-}
data SDNBitCompParams = SDNBitCompParams {
      nBitCompStartBit :: Int32 -- ^ Specifies the leftmost position of the variable-length
                                --   bit field to be written. For example, in the bit field described in the preceding
                                --   paragraph a @nBitCompStartBit@ parameter set to 4 would correspond to the fourth bit
                                --   value of 1 from the right.
    , nBitCompBitLen   :: Int32 -- ^ Specifies the number of bits of the variable-length bit
                                --   field to be written. This number includes the starting bit and the count proceeds
                                --   toward the right end of the bit field - toward the lower-bit numbers. For
                                --   example, starting at bit 5 and writing 4 bits of the bit field described in the
                                --   preceding paragraph would result in the bit field @1110@ being written to the data
                                --   set. This would correspond to a @nBitCompStartBit@ value of 5 and a @nBitCompBitLen@ value of 4.
    , nBitCompSignExt  :: Bool  -- ^ Specifies whether to use the leftmost bit of the
                                --   variable-length bit field to sign-extend to the leftmost bit of the data set data.
                                --   For example, if 9-bit signed integer data is extracted from bits 17-25 and the bit
                                --   in position 25 is 1, then when the data is read back from disk, bits 26-31 will be
                                --   set to 1. Otherwise bit 25 will be 0 and bits 26-31 will be set to 0. USe 'True' to sign-extend.
    , nBitCompFillOne  :: Bool  -- ^ Specifies whether to fill the "background" bits with the value 1 or 0.
                                --
                                --   The "background" bits of a variable-length data set are the bits that fall outside
                                --   of the variable-length bit field stored on disk. For example, if five bits of an
                                --   unsigned 16-bit integer data set located in bits 5 to 9 are written to disk with
                                --   the @nBitCompFillOne@ set to 'True', then when the data is reread into
                                --   memory bits 0 to 4 and 10 to 15 would be set to 1. If the same 5-bit data was
                                --   written with a @nBitCompFillOne@ value of 'False', then bits 0 to 4 and 10 to 15 would
                                --   be set to 0.
                                --
                                --   This bit operation is performed before the sign-extend bit-filling. For example,
                                --   using the sign_ext example above, bits 0 to 16 and 26 to 31 will first be set to
                                --   the "background" bit value, and then bits 26 to 31 will be set to 1 or 0 based on
                                --   the value of the 25th bit.
} deriving (Show, Eq)

{- | Specifies a non-standard bit length for the data set values.

'sd_setnbitdataset' allows the HDF user to specify that a data set contains data
of a non-standard length.
-}
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

{- | Sets the size of the chunk cache.

By default, when a generic data set is promoted to be a chunked data set, the
size of chunk cache is set to the number of chunks along the fastest changing
dimension and a cache for the chunks is created.

If the chunk cache is full and the value of the parameter maxcache is greater
then the current maxcache value, then the chunk cache is reset to the new value
of maxcache. Otherwise the chunk cache remains at the current value of
maxcache. If the chunk cache is not full, then the chunk cache is set to the new
value of maxcache only if the new maxcache value is greater than the current
number of chunks in the cache.

Do not set the value of maxcache to be less than the number of chunks along
the fastest-changing dimension of the biggest slab to be written or read via
'sd_readdata' or 'sd_writedata'. Doing this will cause internal thrashing. See the
section on chunking in Chapter 14, "HDF Performance Issues" in the HDF
User's Guide, for more information on this.
-}
sd_setchunkcache ::
     SDataSetId n t
  -> Int32 -- ^ maxcache
  -> IO (Int32, ()) -- FIXME: return value
sd_setchunkcache (SDataSetId sds_id) cacheSize = do
    h_result <- c_sdsetchunkcache sds_id cacheSize 0
    return $! (fromIntegral h_result, ())

{- | Sets the chunk size and the compression method, if any, of a data set.


'sd_setchunk' makes a data set a chunked data set according to the provided chunking
and compression information.
-}
sd_setchunk :: SDataSetId n t -> HDFChunkParams -> IO (Int32, ())
sd_setchunk (SDataSetId sds_id) chunkParams =
    with chunkParams $ \chunkParamsPtr -> do
        h_result <- c_sdsetchunk sds_id chunkParamsPtr (selectChunkingMode chunkParams)
        return $! (fromIntegral h_result, ())

{- | Retrieves chunking information for a data set. -}
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

{- | Offset and length of a raw data chunk within HDF file.

This information could be retrieved by functions from /Raw data information/ section
and used to read data from HDF file with regular reading and writing routines bypassing HDF library. -}
type RawDataInfo = (Int32, Int32)

{- | Retrieves location and size of annotations' data.

'sd_getanndatainfo' retrieves the location and size specifying the data of
annotations that are of the specific type and are assigned to the provided
object id. There may be more than one annotation, but each annotation has
only one block of data.

The object id can be either an SD interface identifier, returned by
'sd_start' or a data set identifier, returned by 'sd_select'.

__NOTE:__ passing a dimension identifier, returned by 'sd_getdimid', even
though it would typecheck, is __not allowed__ an would lead to HDF error.
-}
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

{- | Retrieves location and size of attribute's data.

SDgetattdatainfo retrieves the offset and length of the data that belongs to an
attribute, which is attached to the HDF4 object specified by object id.

The object id can be either an SD interface identifier, returned by
'sd_start', a data set identifier, returned by 'sd_select', or a dimension
identifier, returned by 'sd_getdimid'.

There are attributes created by 'sd_setattr' and those created by the DFSD API
functions. 'sd_getattdatainfo' can only retrieve data information of attributes
that were created by 'sd_setattr'. If the inquired attribute was created by the
DFSD API functions, 'sd_getoldattdatainfo' should be used to get the attribute's
data information.
-}
sd_getattdatainfo :: SDObjectId id =>
     id
  -> Int32 -- ^ attribute index
  -> IO (Int32, RawDataInfo)
sd_getattdatainfo objId attrId =
    alloca $ \offsetPtr ->
    alloca $ \lengthPtr -> do
        h_result <-c_sdgetattdatainfo (getRawObjectId objId) attrId offsetPtr lengthPtr
        case () of
           _ | h_result == (-1) -> return $! (fromIntegral h_result, (0, 0))
             | h_result == (fromIntegral $ toHDFErrorCode DFE_NOVGREP) -> do
                he_push DFE_NOVGREP "sd_getattdatainfo" "Data.Format.HDF.LowLevel.SD" currentLine
                return $! (-1, (0, 0))
             | otherwise -> do
                offset <- peek offsetPtr
                len <- peek lengthPtr
                return $! (fromIntegral h_result, (offset, len))

{- | Retrieves location and size of old predefined attribute's data.

'sd_getoldattdatainfo' retrieves the offset and length of the data that belongs to
the attribute, which is attached to the SDS or the dimension.

The function only works on attributes that were created by the DFSD API
while its counter part 'sd_getattdatainfo' only works on attributes created with
'sd_setattr'. An application might call 'sd_getattdatainfo' initially. When a
DFSD-created attribute is encountered, 'sd_getattdatainfo' will fail with the
error code DFE_NOVGREP, which indicates there is no vgroup representation for
an SDS (i.e., DFSD API) and the SDS' attributes are stored differently than
when they are created with 'sd_setattr'. The application must call
'sd_getoldattdatainfo' to get the data information of those attributes, if such
error code is detected.

'sd_getoldattdatainfo' takes both SDS identifier and dimension identifier if the
inquired attribute belongs to a dimension. When the inquired attribute belongs
to an SDS, the dimension identifier will not be needed, and should be 'Nothing'.

The attribute is a predefined attribute listed in the following table. Note that
dimensions can only have the first three attributes in the table.

+----------------------+-----------------+
| Predefined Name      | Applicable To   |
+======================+=================+
| @"long_name"@        | Dimension & SDS |
+----------------------+-----------------+
| @"units"@            | Dimension & SDS |
+----------------------+-----------------+
| @"format"@           | Dimension & SDS |
+----------------------+-----------------+
| @"coordsys"@         | Only SDS        |
+----------------------+-----------------+
| @"scale_factor_err"@ | Only SDS        |
+----------------------+-----------------+
| @"add_offset"@       | Only SDS        |
+----------------------+-----------------+
| @"valid_range"@      | Only SDS        |
+----------------------+-----------------+
| @"scale_factor"@     | Only SDS        |
+----------------------+-----------------+
-}
sd_getoldattdatainfo ::
     SDataSetId n t
  -> Maybe SDimensionId
  -> String -- ^ name of a predefined attribute
  -> IO (Int32, RawDataInfo)
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

{- | Retrieves location and size of data blocks in a specified data set, starting at a
specified block.

'sd_getdatainfo' retrieves the offsets and lengths of the blocks of data belonging to
the data set specified by dataset id.

The start block parameter indicates where to start reading the offsets from in
the file. The value for start block must be non-negative and smaller than or
equal to the number of blocks in the data set.

When start block is 0, 'sd_getdatainfo' will start getting data info from
the beginning of the data set's data. When start block is greater than the
number of blocks in the data set, 'sd_getdatainfo' will fail.
-}
sd_getdatainfo ::
     SDataSetId n t
  -> [Int32] -- ^ chunk coordinates, empty list for non-chunked SDS
  -> Int32 -- ^ start block
  -> IO (Int32, [RawDataInfo])
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

{- | Retrieves information about external file and external data of the data set.

If the data set has external element, 'sd_getexternalinfo' will retrieve the name
of the external file, the offset where the data is being stored in the external file,
and the length of the external data. If the data set does not have external
element, 'sd_getexternalinfo' will return empty string as a file name.

__NOTE:__ It is the user's responsibility to ensure that the external files are
kept with the main file prior to accessing the data set with external element.
'sd_getexternalinfo' does not check and the accessing functions will fail if
the external file is missing from the directory where the main file is located.
-}
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

{- | Sets the block size used for storing data sets with unlimited dimensions.

'sd_setblocksize' must be used after calls to 'sd_create' or 'sd_select' and before
the call to 'sd_writedata'.

The block size parameter should be set to a multiple of the desired buffer size.
-}
sd_setblocksize ::
     SDataSetId n t
  -> Int32 -- ^ block size
  -> IO (Int32, ())
sd_setblocksize (SDataSetId sds_id) blockSize = do
    h_result <- c_sdsetblocksize sds_id blockSize
    return $! (fromIntegral h_result, ())

{- | Stores data in an external file.

'sd_setexternalfile' allows users to move the actual data values (i.e., not
metadata) of a data set, into the external data file, and started at the specified
offset. The metadata remains in the original file. Note that this routine works only
with HDF post-version 3.2 files.

Data can only be moved once for any given data set, and it is the user's
responsibility to make sure the external data file is kept with the "original" file.

If the data set already exists, its data will be moved to the external file. Space
occupied by the data in the primary file will not be released. To release the
space in the primary file use the @hdfpack@ command-line utility. If the data set
does not exist, its data will be written to the external file during the consequent
calls to 'sd_writedata'.
-}
sd_setexternalfile :: SDataSetId n t -> String -> Int32 -> IO (Int32, ())
sd_setexternalfile (SDataSetId sds_id) fileName offset =
    withCString fileName $ \fileNamePtr -> do
        h_result <- c_sdsetexternalfile sds_id fileNamePtr offset
        return $! (fromIntegral h_result, ())

{- | Determines whether a dimension has the old and new representations or the
new representation only.

The compatibility mode can be changed by calls to 'sd_setdimval_comp' at any
time between the calls to 'sd_start' and 'sd_end'.
-}
sd_isdimval_bwcomp :: SDimensionId -> IO (Int32, Bool)
sd_isdimval_bwcomp (SDimensionId dimension_id) = do
    h_result <- c_sdisdimval_bwcomp dimension_id
    return $! (fromIntegral h_result, toBool h_result)

{- | Determines whether a dimension will have the old and new representations or
the new representation only.

'sd_setdimval_comp' sets the compatibility mode specified by the comp_mode
parameter for the dimension. The two possible compatibility modes are:
"backward-compatible" mode, which implies that the old and new dimension
representations are written to the file, and "backward-incompatible" mode,
which implies that only the new dimension representation is written to the file.

Unlimited dimensions are always backward-compatible, therefore 'sd_setdimval_comp' takes
no action on unlimited dimensions.

As of HDF version 4.1r1, the default mode is backward-incompatible.

Subsequent calls to 'sd_setdimval_comp' will override the settings established
in previous calls to the routine.
-}
sd_setdimval_comp :: SDimensionId -> Bool -> IO (Int32, ())
sd_setdimval_comp (SDimensionId dimension_id) bwCompatible = do
    h_result <- c_sdsetdimval_comp dimension_id (fromBool bwCompatible)
    return $! (fromIntegral h_result, ())

{- | Stores the values of a dimension.

For fixed-size arrays, the size if set dimension scale of must be equal to
the dimension size or the routine will fail.

__NOTE:__ if 'sd_setdimscale' is called and 'sd_setdimname' is subsequently
called for the same dimension, 'sd_setdimscale' must be called again to
reassociate the scale with the new name.
-}
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

{- | Retrieves the scale values for a dimension.

If dimension scale is not set this routine will fail.

It is not possible to read a subset of the scale values. 'sd_getdimscale' returns all
of the scale values stored with the given dimension.
-}
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

{- | Attaches an attribute to an object.

Attribute name can be any ASCII string with maximum length of 256.
-}
sd_setattr :: (SDObjectId id, Storable a, CanBeAttribute t, a `IsElementOf` t) =>
     id
  -> String -- ^ attribute name
  -> HDataType a
  -> t
  -> IO (Int32, ())
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

{- | Reads the values of an attribute. -}
sd_readattr :: SDObjectId id =>
     id
  -> Int32 -- ^ index of attribute to read
  -> IO (Int32, HDFVector)
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

{- | Writes a data chunk to a chunked data set.

'sd_writechunk' writes the entire chunk of data stored in the buffer to the
chunked data set identified by the dataset id. Writing starts at the
location specified by the origin parameter. 'sd_writechunk' is used when an
entire chunk of data is to be written. 'sd_writedata' is used when the write
operation is to be done regardless of the chunking scheme used in the data set.

The origin parameter specifies the coordinates of the chunk according to the
chunk position in the chunked array. Refer to the Chapter 3, "Scientific Data
Sets (SD API)" of the HDF User's Guide, for a description of the organization
of chunks in a data set.
-}
sd_writechunk :: forall (t :: HDataType a) (n :: Nat). Storable a =>
     SDataSetId n t
  -> [Int32] -- ^ origin of chunk to be written
  -> VS.Vector a
  -> IO (Int32, ())
sd_writechunk (SDataSetId sds_id) chunkCoords dataChunk =
    withArray chunkCoords $ \chunkCoordsPtr ->
    VS.unsafeWith dataChunk $ \dataChunkPtr -> do
        h_result <- c_sdwritechunk
                        sds_id
                        chunkCoordsPtr
                        (castPtr dataChunkPtr)
        return $! (fromIntegral h_result, ())

{- | Reads a data chunk from a chunked data set.

'sd_readchunk' reads the entire chunk of data from the chunked data set
identified by the dataset id, and stores the data in the buffer.
Reading starts at the location specified by the origin parameter. 'sd_readchunk'
is used when an entire chunk of data is to be read. 'sd_readdata' is used when
the read operation is to be done regardless of the chunking scheme used in the
data set.

The origin parameter specifies the coordinates of the chunk according to the
chunk position in the chunked array. Refer to the Chapter 3, "Scientific Data
Sets (SD API)" of the HDF User's Guide, for a description of the organization
of chunks in a data set.
-}
sd_readchunk :: forall (t :: HDataType a) (n :: Nat). Storable a =>
     SDataSetId n t
  -> [Int32] -- ^ origin of chunk to be read
  -> IO (Int32, VS.Vector a)
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

{- | Reads a subsample of data from a data set or coordinate variable.

When reading data from a "chunked" data set using 'sd_readdata',
consideration should be given to the issues presented in the section on
chunking in Chapter 3, "Scientific Data Sets (SD API)" and Chapter 14, "HDF
Performance Issues" in the HDF User's Guide.
-}
sd_readdata :: forall (t :: HDataType a) (n :: Nat). (Storable a, KnownNat n) =>
     SDataSetId n t
  -> Index n -- ^ starting location from where data is read
  -> Index n -- ^ interval between the values that will be read along each dimension
  -> Index n -- ^ number of values to be read along each dimension
  -> IO (Int32, VS.Vector a)
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

{- | Writes a subsample of data to a data set or to a coordinate variable.

When writing data to a chunked data set using 'sd_writedata',
consideration should be given to the issues presented in the section on
chunking in Chapter 3, "Scientific Data Sets (SD API)" and Chapter 14, "HDF
Performance Issues" in the HDF User's Guide.
-}
sd_writedata :: forall (t :: HDataType a) (n :: Nat). (Storable a, KnownNat n) =>
     SDataSetId n t
  -> Index n -- ^ starting location of the data to be written
  -> Index n -- ^ number of values to skip along each dimension
  -> Index n -- ^ number of values to be written along each dimension
  -> VS.Vector a
  -> IO (Int32, ())
sd_writedata (SDataSetId sds_id) start stride edges sdsData =
    withIndex start $ \startPtr ->
    withIndex stride $ \stridePtr ->
    withIndex edges $ \edgesPtr ->
    VS.unsafeWith sdsData $ \sdsDataPtr -> do
        h_result <- c_sdwritedata sds_id startPtr stridePtr edgesPtr (castPtr sdsDataPtr)
        return $! (fromIntegral h_result, ())
