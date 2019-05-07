{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Format.HDF.LowLevel.SD where

import           Data.Int
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Ptr

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
-- SDcheckempty
-- SDfileinfo
-- SDgetfilename
-- SDgetinfo
-- SDget_maxopenfiles
-- SDgetnamelen
-- SDget_numopenfiles
-- SDgetnumvars_byname
-- SDidtoref
-- SDidtype
-- SDiscoordvar
-- SDisrecord
-- SDnametoindex
-- SDnametoindices
-- SDreftoindex
-- SDreset_maxopenfiles

-- Dimensions
-- SDdiminfo
-- SDgetdimid
-- SDsetdimname

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

newtype SDId = SDId Int32
newtype SDataSetId = SDataSetId Int32

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