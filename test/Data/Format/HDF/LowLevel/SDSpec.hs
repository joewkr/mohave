{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Data.Format.HDF.LowLevel.SDSpec(spec) where

import           Data.Int (Int32)
import           Data.Proxy
import           Data.Word (Word8, Word16)
import           Test.Hspec
import           GHC.TypeNats
import           Data.Type.Equality ((:~:)(Refl))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Format.HDF.LowLevel.C.Definitions
import           Data.Format.HDF.LowLevel.Definitions
import           Data.Format.HDF.LowLevel.SD
import qualified Data.Vector.Storable as VS
import           Foreign.Ptr (castPtr)
import           System.IO
import           System.Directory (copyFileWithMetadata)
import           Testing.Common

spec :: Spec
spec = do
    describe "SD access routines" $ do
        it "opens existing HDF file" $ do
            sd_id                  <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
            _                      <- check =<< sd_end sd_id
            return ()
        it "correctly handles missing HDF file" $ do
            (status_1, sd_id)      <-           sd_start "test-data/sd/NULL" hdf_read
            (status_2, _)          <-           sd_end sd_id
            status_1 `shouldBe` (-1)
            status_2 `shouldBe` (-1)
        it "correctly creates new SDS" $ do
            sd_id                  <- check =<< sd_start "empty_sds.hdf" hdf_create
            _                      <- check =<< sd_create sd_id "emptyDataSet" HFloat64 (D 1 :| 2 :| 3)
            _                      <- check =<< sd_end sd_id
            return ()
        it "correctly selects existing SDS" $ do
            sd_id                  <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
            (SomeSDS _ sds_id)     <- check =<< sd_select sd_id 0
            _                      <- check =<< sd_endaccess sds_id
            _                      <- check =<< sd_end sd_id
            return ()
        it "correctly handles missing SDS" $ do
            sd_id                  <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
            (status_1, SomeSDS _ sds_id) <-     sd_select sd_id 999
            (status_2, _)          <-           sd_endaccess sds_id
            _                      <- check =<< sd_end sd_id
            status_1 `shouldBe` (-1)
            status_2 `shouldBe` (-1)
    describe "SD General inquiry" $ do
        context "SDcheckempty" $ do
            it "empty SDS" $ do
                sd_id              <- check =<< sd_start "test-data/sd/emptySDSs.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                is_empty           <- check =<< sd_checkempty sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                is_empty `shouldBe` True
            it "non-empty SDS" $ do
                sd_id              <- check =<< sd_start "test-data/sd/emptySDSs.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 1
                is_empty           <- check =<< sd_checkempty sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                is_empty `shouldBe` False
            it "incorrect SDS" $ do
                sd_id              <- check =<< sd_start "test-data/sd/emptySDSs.hdf" hdf_read
                (status_1, SomeSDS _ sds_id) <- sd_select sd_id 999
                (status_2, is_empty) <-         sd_checkempty sds_id
                (status_3, _)      <-           sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                status_1 `shouldBe` (-1)
                status_2 `shouldBe` (-1)
                status_3 `shouldBe` (-1)
                is_empty `shouldBe` True
        context "SDfileinfo" $ do
            it "test1.hdf" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                fileInfo           <- check =<< sd_fileinfo sd_id
                _                  <- check =<< sd_end sd_id
                fileInfo `shouldBe` (9,1)
            it "test2.hdf" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test2.hdf" hdf_read
                fileInfo           <- check =<< sd_fileinfo sd_id
                _                  <- check =<< sd_end sd_id
                fileInfo `shouldBe` (1,0)
        context "SDgetnamelen" $ do
            it "file name length" $ do
                sd_id          <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                fileNameLength <- check =<< sd_getnamelen sd_id
                _              <- check =<< sd_end sd_id
                fileNameLength `shouldBe` 22
            it "dataset name length" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                fileNameLength     <- check =<< sd_getnamelen sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                fileNameLength `shouldBe` 12
            it "dimension name length" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                dim_id             <- check =<< sd_getdimid sds_id 0
                fileNameLength     <- check =<< sd_getnamelen dim_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                fileNameLength `shouldBe` 5
        context "SDgetfilename" $ do
            it "gets file name of correctly opened dataset" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                fileName           <- check =<< sd_getfilename sd_id
                _                  <- check =<< sd_end sd_id
                fileName `shouldBe` "test-data/sd/test1.hdf"
        context "SDgetinfo" $ do
            it "regular SDS" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                sdsInfo            <- check =<< sd_getinfo sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                sdsInfo `shouldBe` (SDataSetInfoRaw
                    "DataSetAlpha"
                    2 [4,8] (HDFValue HFloat32 ()) 6)
            it "long name SDS" $ do
                sd_id              <- check =<< sd_start "test-data/sd/SDSlongname.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                sdsInfo            <- check =<< sd_getinfo sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                sdsInfo `shouldBe` (SDataSetInfoRaw
                    "The name of this dataset is long, and it is used to test the new variable length name feature"
                    2 [10,10] (HDFValue HInt32 ()) 0)
        context "SDget_maxopenfiles" $ do
            it "reports reasonable limits" $ do
                (curr_max, sys_limit) <- check =<< sd_get_maxopenfiles
                curr_max `shouldSatisfy` (> 0)
                curr_max `shouldSatisfy` (<= sys_limit)
                sys_limit `shouldSatisfy` (> 0)
            it "reflects updated limits" $ do
                new_limit          <- check =<< sd_reset_maxopenfiles 128
                (curr_max, _)      <- check =<< sd_get_maxopenfiles
                (curr_max == new_limit) `shouldBe` True
        context "SDget_numopenfiles" $ do
            it "no open files" $ do
                numFiles <- check =<< sd_get_numopenfiles
                numFiles `shouldBe` 0
            it "some open files" $ do
                sd_id_1            <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                sd_id_2            <- check =<< sd_start "test-data/sd/test2.hdf" hdf_read
                numFiles           <- check =<< sd_get_numopenfiles
                _                  <- check =<< sd_end sd_id_1
                _                  <- check =<< sd_end sd_id_2
                numFiles `shouldBe` 2
        context "SDgetnumvars_byname" $ do
            it "reports multiple variables" $ do
                sd_id              <- check =<< sd_start "test-data/sd/vars_samename.hdf" hdf_read
                numVars            <- check =<< sd_getnumvars_byname sd_id "Common Name"
                _                  <- check =<< sd_end sd_id
                numVars `shouldBe` 3
            it "reports unique variable" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                numVars            <- check =<< sd_getnumvars_byname sd_id "DataSetAlpha"
                _                  <- check =<< sd_end sd_id
                numVars `shouldBe` 1
        context "SDidtoref" $ do
            it "reports reference number for SDS" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                _                  <- check =<< sd_idtoref sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                return ()
        context "SDiscoordvar" $ do
            it "detects coordinate variable" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 2
                isCoordVar         <- check =<< sd_iscoordvar sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                isCoordVar `shouldBe` True
            it "detects non-coordinate variable" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                isCoordVar         <- check =<< sd_iscoordvar sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                isCoordVar `shouldBe` False
        context "SDisrecord" $ do
            it "detects variable with unlimited dimension" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 7
                isRecord           <- check =<< sd_isisrecord sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                isRecord `shouldBe` True
            it "detects variable without unlimited dimension" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                isRecord           <- check =<< sd_isisrecord sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                isRecord `shouldBe` False
        context "SDnametoindex" $ do
            it "reports index of coordinate variable" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                sds_index          <- check =<< sd_nametoindex sd_id "MyDim"
                _                  <- check =<< sd_end sd_id
                sds_index `shouldBe` 2
            it "reports index of non-coordinate variable" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                sds_index          <- check =<< sd_nametoindex sd_id "DataSetAlpha"
                _                  <- check =<< sd_end sd_id
                sds_index `shouldBe` 0
        context "SDnametoindices" $ do
            it "reports unique variable" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                hdfVarList         <- check =<< sd_nametoindices sd_id "DataSetAlpha"
                _                  <- check =<< sd_end sd_id
                hdfVarList `shouldBe` [HDFVarList 0 0]
            it "reports multiple variables" $ do
                sd_id              <- check =<< sd_start "test-data/sd/vars_samename.hdf" hdf_read
                hdfVarList         <- check =<< sd_nametoindices sd_id "Common Name"
                _                  <- check =<< sd_end sd_id
                hdfVarList `shouldBe` [HDFVarList 0 0, HDFVarList 1 0, HDFVarList 3 1]
        context "SDreftoindex" $ do
            it "correctly converts reference number to SDS id" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                sds_ref            <- check =<< sd_idtoref sds_id
                sds_index_from_ref <- check =<< sd_reftoindex sd_id sds_ref
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                sds_index_from_ref `shouldBe` 0
        context "SDreset_maxopenfiles" $ do
            it "sets new limit for open hdf files" $ do
                old_limit          <- check =<< sd_reset_maxopenfiles 0
                _                  <- check =<< sd_reset_maxopenfiles 128
                _                  <- check =<< sd_reset_maxopenfiles old_limit
                return ()
    describe "Dimensions" $ do
        context "SDgetdimid" $ do
            it "returns dimension id" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                _                  <- check =<< sd_getdimid sds_id 0
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                return ()
            it "reports error on missing dimension" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                (status, _)        <-           sd_getdimid sds_id 999
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                status `shouldBe` (-1)
        context "SDdiminfo" $ do
            it "returns dimension information" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                dim_id             <- check =<< sd_getdimid sds_id 0
                dimInfo            <- check =<< sd_diminfo dim_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                dimInfo `shouldBe` (SDimensionInfoRaw
                                    "MyDim"
                                    4 (HDFValue HInt32 ()) 4)
            it "returns unlimited dimension information" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                sds_index          <- check =<< sd_nametoindex sd_id "dimval_1_compat"
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id sds_index
                dim_id             <- check =<< sd_getdimid sds_id 0
                dimInfo            <- check =<< sd_diminfo dim_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                dimInfo `shouldBe` (SDimensionInfoRaw
                                    "fakeDim8"
                                    0 (HDFValue HNone ()) 0)
        context "SDsetdimname" $ do
            it "sets new dimension name" $ do
                sd_id              <- check =<< sd_start "empty_sds.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HFloat64 (D 1 :| 2 :| 3)
                dim_id             <- check =<< sd_getdimid sds_id 0
                _                  <- check =<< sd_setdimname dim_id "MyDim"
                dimInfo            <- check =<< sd_diminfo dim_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                dimInfo `shouldBe` (SDimensionInfoRaw
                                    "MyDim"
                                    1 (HDFValue HNone ()) 0)
    context "Dimension scales" $ do
        context "SDsetdimscale" $ do
            it "sets dimension scale - 1" $ do
                sd_id              <- check =<< sd_start "dimension_scale_1.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HFloat64 (D 5 :| 2 :| 3)
                dim_id             <- check =<< sd_getdimid sds_id 0
                _                  <- check =<< sd_setdimname dim_id "MyDim"
                _                  <- check =<< sd_setdimscale dim_id HInt32 (VS.fromList [1,2,3,4,5])
                dimInfo            <- check =<< sd_diminfo dim_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                dimInfo `shouldBe` (SDimensionInfoRaw
                                    "MyDim"
                                    5 (HDFValue HInt32 ()) 0)
            it "sets dimension scale - 2" $ do
                let origDimScale = VS.fromList [5,4,3,2,1]
                sd_id              <- check =<< sd_start "dimension_scale_2.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HFloat64 (D 5 :| 2 :| 3)
                dim_id             <- check =<< sd_getdimid sds_id 0
                _                  <- check =<< sd_setdimname dim_id "MyDim"
                _                  <- check =<< sd_setdimscale dim_id HInt32 origDimScale
                (HDFValue t v)     <- check =<< sd_getdimscale sds_id dim_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                case t of
                    HInt32 -> v `shouldBe` origDimScale
                    _ -> expectationFailure "Unexpected dimension data type"
        context "SDgetdimscale" $ do
            it "gets dimension scale" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                dim_id             <- check =<< sd_getdimid sds_id 0
                (HDFValue t v)     <- check =<< sd_getdimscale sds_id dim_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                case t of
                    HInt32 -> v `shouldBe` (VS.fromList [1,5,7,24])
                    _ -> expectationFailure "Unexpected dimension data type"
            it "gets unlimited dimension scale" $ do
                let sdsData     = VS.fromList ([4,5,6,7] :: [Word8])
                    sdsDimScale = VS.fromList ([1,2,3,4] :: [Word16])
                sd_id              <- check =<< sd_start "unlimited_dim_scale.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "DataSet" HWord8 (D 0)
                _                  <- check =<< sd_writedata sds_id (D 0) (D 1) (D 4) sdsData
                dim_id             <- check =<< sd_getdimid sds_id 0
                _                  <- check =<< sd_setdimname dim_id "MyDim"
                _                  <- check =<< sd_setdimscale dim_id HWord16 sdsDimScale
                (HDFValue t v)     <- check =<< sd_getdimscale sds_id dim_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                case t of
                    HWord16 -> v `shouldBe` sdsDimScale
                    _ -> expectationFailure "Unexpected dimension data type"
    context "User-defined attributes" $ do
        context "SDfindattr" $ do
            it "finds global attribute" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                attr_index         <- check =<< sd_findattr sd_id "F-attr"
                _                  <- check =<< sd_end sd_id
                attr_index `shouldBe` 0
            it "finds SDS attribute" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                attr_index         <- check =<< sd_findattr sds_id "units"
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                attr_index `shouldBe` 4
            it "finds dimension attribute" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                dim_id             <- check =<< sd_getdimid sds_id 0
                attr_index         <- check =<< sd_findattr dim_id "format"
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                attr_index `shouldBe` 2
        context "SDattrinfo" $ do
            it "returns global attribute information" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                attr_index         <- check =<< sd_findattr sd_id "F-attr"
                attrInfo           <- check =<< sd_attrinfo sd_id attr_index
                _                  <- check =<< sd_end sd_id
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "F-attr"
                                     10
                                     (HDFValue HChar8 ()))
            it "returns SDS attribute information" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                attr_index         <- check =<< sd_findattr sds_id "valid_range"
                attrInfo           <- check =<< sd_attrinfo sds_id attr_index
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "valid_range"
                                     2
                                     (HDFValue HFloat32 ()))
            it "returns dimension attribute information" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                dim_id             <- check =<< sd_getdimid sds_id 0
                attr_index         <- check =<< sd_findattr dim_id "DimAttr"
                attrInfo           <- check =<< sd_attrinfo dim_id attr_index
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "DimAttr"
                                     1
                                     (HDFValue HFloat32 ()))
        context "SDsetattr" $ do
            it "sets global attribute - ByteString" $ do
                let testAttr = BS8.pack "Test attribute - 1"
                sd_id              <- check =<< sd_start "global_attr_1.hdf" hdf_create
                _                  <- check =<< sd_setattr sd_id "Test_1" HChar8 testAttr
                attrInfo           <- check =<< sd_attrinfo sd_id 0
                _                  <- check =<< sd_end sd_id
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_1"
                                     18
                                     (HDFValue HChar8 ()))
            it "sets global attribute - ByteString UChar" $ do
                let testAttr = BS8.pack "Test attribute - 1"
                sd_id              <- check =<< sd_start "global_attr_2.hdf" hdf_create
                _                  <- check =<< sd_setattr sd_id "Test_1" HUChar8 testAttr
                (HDFValue t v)     <- check =<< sd_readattr sd_id 0
                attrInfo           <- check =<< sd_attrinfo sd_id 0
                _                  <- check =<< sd_end sd_id
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_1"
                                     18
                                     (HDFValue HUChar8 ()))
                case t of
                    HUChar8 -> do
                        strAttr <- VS.unsafeWith v $ \vPtr -> BS.packCStringLen (castPtr vPtr, VS.length v)
                        strAttr `shouldBe` testAttr
                    _ -> expectationFailure "Unexpected dimension data type"
            it "sets global attribute - List" $ do
                let testAttr = [1..10] :: [Int32]
                sd_id              <- check =<< sd_start "global_attr_3.hdf" hdf_create
                _                  <- check =<< sd_setattr sd_id "Test_2" HInt32 testAttr
                attrInfo           <- check =<< sd_attrinfo sd_id 0
                _                  <- check =<< sd_end sd_id
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_2"
                                     10
                                     (HDFValue HInt32 ()))
            it "sets global attribute - Vector" $ do
                let testAttr = VS.fromList ([-10..10] :: [Float])
                sd_id              <- check =<< sd_start "global_attr_4.hdf" hdf_create
                _                  <- check =<< sd_setattr sd_id "Test_2" HFloat32 testAttr
                attrInfo           <- check =<< sd_attrinfo sd_id 0
                _                  <- check =<< sd_end sd_id
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_2"
                                     21
                                     (HDFValue HFloat32 ()))
            it "sets SDS attribute - ByteString" $ do
                let testAttr = BS8.pack "Test attribute - 1"
                sd_id              <- check =<< sd_start "sds_attr_1.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HFloat64 (D 1 :| 2 :| 3)
                _                  <- check =<< sd_setattr sds_id "Test_1" HChar8 testAttr
                attrInfo           <- check =<< sd_attrinfo sds_id 0
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_1"
                                     18
                                     (HDFValue HChar8 ()))
            it "sets SDS attribute - ByteString UChar" $ do
                let testAttr = BS8.pack "Test attribute - 1"
                sd_id              <- check =<< sd_start "sds_attr_2.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HFloat64 (D 1 :| 2 :| 3)
                _                  <- check =<< sd_setattr sds_id "Test_1" HUChar8 testAttr
                attrInfo           <- check =<< sd_attrinfo sds_id 0
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_1"
                                     18
                                     (HDFValue HUChar8 ()))
            it "sets SDS attribute - List" $ do
                let testAttr = [1..10] :: [Int32]
                sd_id              <- check =<< sd_start "sds_attr_3.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HFloat64 (D 1 :| 2 :| 3)
                _                  <- check =<< sd_setattr sds_id "Test_2" HInt32 testAttr
                attrInfo           <- check =<< sd_attrinfo sds_id 0
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_2"
                                     10
                                     (HDFValue HInt32 ()))
            it "sets SDS attribute - Vector" $ do
                let testAttr = VS.fromList ([-10..10] :: [Float])
                sd_id              <- check =<< sd_start "sds_attr_4.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HFloat64 (D 1 :| 2 :| 3)
                _                  <- check =<< sd_setattr sds_id "Test_2" HFloat32 testAttr
                attrInfo           <- check =<< sd_attrinfo sds_id 0
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_2"
                                     21
                                     (HDFValue HFloat32 ()))
            it "sets dimension attribute - ByteString" $ do
                let testAttr = BS8.pack "Test attribute - 1"
                sd_id              <- check =<< sd_start "dim_attr_1.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HFloat64 (D 1 :| 2 :| 3)
                dim_id             <- check =<< sd_getdimid sds_id 0
                _                  <- check =<< sd_setattr dim_id "Test_1" HChar8 testAttr
                attrInfo           <- check =<< sd_attrinfo dim_id 0
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_1"
                                     18
                                     (HDFValue HChar8 ()))
            it "sets dimension attribute - ByteString UChar" $ do
                let testAttr = BS8.pack "Test attribute - 1"
                sd_id              <- check =<< sd_start "dim_attr_2.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HFloat64 (D 1 :| 2 :| 3)
                dim_id             <- check =<< sd_getdimid sds_id 0
                _                  <- check =<< sd_setattr dim_id "Test_1" HUChar8 testAttr
                attrInfo           <- check =<< sd_attrinfo dim_id 0
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_1"
                                     18
                                     (HDFValue HUChar8 ()))
            it "sets dimension attribute - List" $ do
                let testAttr = [1..10] :: [Int32]
                sd_id              <- check =<< sd_start "dim_attr_3.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HFloat64 (D 1 :| 2 :| 3)
                dim_id             <- check =<< sd_getdimid sds_id 0
                _                  <- check =<< sd_setattr dim_id "Test_2" HInt32 testAttr
                attrInfo           <- check =<< sd_attrinfo dim_id 0
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_2"
                                     10
                                     (HDFValue HInt32 ()))
            it "sets dimension attribute - Vector" $ do
                let testAttr = VS.fromList ([-10..10] :: [Float])
                sd_id              <- check =<< sd_start "dim_attr_4.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HFloat64 (D 1 :| 2 :| 3)
                dim_id             <- check =<< sd_getdimid sds_id 0
                _                  <- check =<< sd_setattr dim_id "Test_2" HFloat32 testAttr
                attrInfo           <- check =<< sd_attrinfo dim_id 0
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_2"
                                     21
                                     (HDFValue HFloat32 ()))
        context "SDreadattr" $ do
            it "reads global attribute" $ do
                let expectedData = BS8.pack "globulator"
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (HDFValue t v)     <- check =<< sd_readattr sd_id 0
                _                  <- check =<< sd_end sd_id
                case t of
                    HChar8 -> do
                        strAttr <- VS.unsafeWith v $ \vPtr -> BS.packCStringLen (castPtr vPtr, VS.length v)
                        strAttr `shouldBe` expectedData
                    _ -> expectationFailure "Unexpected dimension data type"
            it "reads SDS attribute" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                attr_index         <- check =<< sd_findattr sds_id "valid_range"
                (HDFValue t v)     <- check =<< sd_readattr sds_id attr_index
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                case t of
                    HFloat32 -> v `shouldBe` (VS.fromList [4.5999999, 10.0])
                    _ -> expectationFailure "Unexpected dimension data type"
            it "reads dimension attribute" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                dim_id             <- check =<< sd_getdimid sds_id 0
                attr_index         <- check =<< sd_findattr dim_id "DimAttr"
                (HDFValue t v)     <- check =<< sd_readattr dim_id attr_index
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                case t of
                    HFloat32 -> v `shouldBe` (VS.fromList [3.1415])
                    _ -> expectationFailure "Unexpected dimension data type"
    context "Predefined attributes" $ do
        context "SDgetcal" $ do
            it "reports calibration parameters" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 1
                calibrationParams  <- check =<< sd_getcal sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                calibrationParams `shouldBe`
                    (SCalibrationParametersRaw 1.0 5.0 3.0 2.5 (HDFValue HInt8 ()))
            it "handles missing calibration parameters" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                (status, _)        <-           sd_getcal sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                status `shouldBe` (-1)
        context "SDgetdatastrs" $ do
            it "returns predefined string attributes" $ do
                sd_id              <- check =<< sd_start "test-data/sd/tdfsdatts.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                defaultStringAttrs <- check =<< sd_getdatastrs sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                defaultStringAttrs `shouldBe`
                    (SDsetDescStringsRaw
                        "Datalabel"
                        "Dataunit"
                        "Datafmt"
                        "coordsys")
        context "SDgetdimstrs" $ do
            it "returns predefined string attributes" $ do
                sd_id              <- check =<< sd_start "test-data/sd/dim.hdf" hdf_read
                sds_index          <- check =<< sd_nametoindex sd_id "HDF Data 2"
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id sds_index
                dim_id             <- check =<< sd_getdimid sds_id 0
                defaultStringAttrs <- check =<< sd_getdimstrs dim_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                defaultStringAttrs `shouldBe`
                    (SDimDescStringsRaw
                        "DimLabel"
                        "Units"
                        "TheFormat")
        context "SDgetfillvalue" $ do
            it "returns correct Float fill value" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS t sds_id) <- check =<< sd_select sd_id 0
                case t of
                    HFloat32 -> do
                        fillValue <- check =<< sd_getfillvalue sds_id
                        _         <- check =<< sd_endaccess sds_id
                        _         <- check =<< sd_end sd_id
                        fillValue `shouldBe` (-17.5 :: Float)
                    _ -> expectationFailure "Unexpected SDS data type"
            it "returns correct Int fill value" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                sds_index          <- check =<< sd_nametoindex sd_id "FIXED1"
                (SomeSDS t sds_id) <- check =<< sd_select sd_id sds_index
                case t of
                    HInt32 -> do
                        fillValue <- check =<< sd_getfillvalue sds_id
                        _         <- check =<< sd_endaccess sds_id
                        _         <- check =<< sd_end sd_id
                        fillValue `shouldBe` (-300 :: Int32)
                    _ -> expectationFailure "Unexpected SDS data type"
        context "SDgetrange" $ do
            it "returns correct range" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS t sds_id) <- check =<< sd_select sd_id 0
                case t of
                    HFloat32 -> do
                        validRange <- check =<< sd_getrange sds_id
                        _          <- check =<< sd_endaccess sds_id
                        _          <- check =<< sd_end sd_id
                        validRange `shouldBe` ((4.6, 10.0) :: (Float, Float))
                    _ -> expectationFailure "Unexpected SDS data type"
        context "SDsetcal" $ do
            it "sets calibration parameters" $ do
                let calibrationParamsNew = SCalibrationParametersRaw 1.0 5.0 3.0 2.5 (HDFValue HFloat64 ())
                sd_id              <- check =<< sd_start "empty_sds.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HWord8 (D 1 :| 2 :| 3)
                _                  <- check =<< sd_setcal sds_id calibrationParamsNew
                calibrationParams  <- check =<< sd_getcal sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                calibrationParams `shouldBe` calibrationParamsNew
        context "SDsetdatastrs" $ do
            it "sets all predefined string attributes" $ do
                let defaultStringAttrsNew = SDsetDescStringsRaw "Label" "Unit" "Format" "Coordinate system"
                sd_id              <- check =<< sd_start "empty_sds.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HWord8 (D 1 :| 2 :| 3)
                _                  <- check =<< sd_setdatastrs sds_id defaultStringAttrsNew
                defaultStringAttrs <- check =<< sd_getdatastrs sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                defaultStringAttrs `shouldBe` defaultStringAttrsNew
            it "sets some predefined string attributes" $ do
                let defaultStringAttrsNew = SDsetDescStringsRaw "Label" "Unit" "" ""
                sd_id              <- check =<< sd_start "empty_sds.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HWord8 (D 1 :| 2 :| 3)
                _                  <- check =<< sd_setdatastrs sds_id defaultStringAttrsNew
                defaultStringAttrs <- check =<< sd_getdatastrs sds_id
                -- Check that we don't create empty attribute
                (status, _)        <-           sd_findattr sds_id "format"
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                status `shouldBe` (-1)
                defaultStringAttrs `shouldBe` defaultStringAttrsNew
        context "SDsetdimstrs" $ do
            it "sets all predefined string attributes" $ do
                let defaultStringAttrsNew = SDimDescStringsRaw "Label" "Unit" "Format"
                sd_id              <- check =<< sd_start "empty_sds.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HWord8 (D 1 :| 2 :| 3)
                dim_id             <- check =<< sd_getdimid sds_id 0
                _                  <- check =<< sd_setdimstrs dim_id defaultStringAttrsNew
                defaultStringAttrs <- check =<< sd_getdimstrs dim_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                defaultStringAttrs `shouldBe` defaultStringAttrsNew
            it "sets some predefined string attributes" $ do
                let defaultStringAttrsNew = SDimDescStringsRaw "Label" "Unit" ""
                sd_id              <- check =<< sd_start "empty_sds.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HWord8 (D 1 :| 2 :| 3)
                dim_id             <- check =<< sd_getdimid sds_id 0
                _                  <- check =<< sd_setdimstrs dim_id defaultStringAttrsNew
                defaultStringAttrs <- check =<< sd_getdimstrs dim_id
                -- Check that we don't create empty attribute
                (status, _)        <-           sd_findattr sds_id "format"
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                status `shouldBe` (-1)
                defaultStringAttrs `shouldBe` defaultStringAttrsNew
        context "SDsetfillvalue" $ do
            it "sets fill value" $ do
                sd_id              <- check =<< sd_start "empty_sds.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HInt32 (D 1 :| 2 :| 3)
                _                  <- check =<< sd_setfillvalue sds_id (-777 :: Int32)
                fillValue          <- check =<< sd_getfillvalue sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                fillValue `shouldBe` (-777 :: Int32)
        context "SDsetfillmode" $ do
            it "sets fill mode" $ do
                let sdsData = VS.fromList ([7] :: [Word8] )
                    expectedSdsData = VS.fromList ([5,7] :: [Word8] )
                sd_id              <- check =<< sd_start "sds_fill_1.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "DataSet" HWord8 (D 2)
                _                  <- check =<< sd_setfillmode sd_id hdf_fill
                _                  <- check =<< sd_setfillvalue sds_id 5
                _                  <- check =<< sd_writedata sds_id (D 1) (D 1) (D 1) sdsData
                v                  <- check =<< sd_readdata sds_id (D 0) (D 1) (D 2)
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                v `shouldBe` expectedSdsData
            it "sets no-fill mode" $ do
                let sdsData = VS.fromList ([7] :: [Word8] )
                    expectedSdsData = VS.fromList ([5,7] :: [Word8] )
                sd_id              <- check =<< sd_start "sds_nofill_1.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "DataSet" HWord8 (D 2)
                _                  <- check =<< sd_setfillmode sd_id hdf_nofill
                _                  <- check =<< sd_setfillvalue sds_id 5
                _                  <- check =<< sd_writedata sds_id (D 1) (D 1) (D 1) sdsData
                v                  <- check =<< sd_readdata sds_id (D 0) (D 1) (D 2)
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                v `shouldNotBe` expectedSdsData
        context "SDsetrange" $ do
            it "sets valid range for dataset" $ do
                sd_id              <- check =<< sd_start "empty_sds.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HInt32 (D 1 :| 2 :| 3)
                _                  <- check =<< sd_setrange sds_id (0 :: Int32) (100 :: Int32)
                validRange         <- check =<< sd_getrange sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                validRange `shouldBe` ((0, 100) :: (Int32, Int32))
    context "Compression" $ do
        context "SDsetcompress/SDgetcompinfo" $ do
            it "sets compression parameters" $ do
                let compParamsNew = HDFCompDeflate 9
                sd_id              <- check =<< sd_start "empty_sds.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HInt32 (D 1 :| 2 :| 3)
                _                  <- check =<< sd_setcompress sds_id compParamsNew
                compParams         <- check =<< sd_getcompinfo sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                compParams `shouldBe` compParamsNew
        context "SDsetnbitdataset" $ do
            it "sets n-bit compression parameters" $ do
                let compParamsNew = SDNBitCompParams 0 3 False False
                sd_id              <- check =<< sd_start "empty_sds.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HInt32 (D 1 :| 2 :| 3)
                _                  <- check =<< sd_setnbitdataset sds_id compParamsNew
                compParams         <- check =<< sd_getcompinfo sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                compParams `shouldBe` HDFCompNBit (fromHDataType HInt32) 0 0 0 3
    context "Chunking/Tiling" $ do
        context "SDgetchunkinfo" $ do
            it "handles empty SDS" $ do
                sd_id              <- check =<< sd_start "empty_sds.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HInt32 (D 1 :| 2 :| 3)
                chunkParams        <- check =<< sd_getchunkinfo sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                chunkParams `shouldBe` (HDFChunkParams [] HDFCompNone)
            it "returns correct chunking information" $ do
                sd_id              <- check =<< sd_start "test-data/sd/chktst.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                chunkParams        <- check =<< sd_getchunkinfo sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                chunkParams `shouldBe` (HDFChunkParams [3,2] HDFCompNone)
            it "returns correct chunking information from compressed SDS - 1" $ do
                sd_id              <- check =<< sd_start "test-data/sd/chktst.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 5
                chunkParams        <- check =<< sd_getchunkinfo sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                chunkParams `shouldBe` (HDFChunkParams [1,1,4] $ HDFCompSkHuff 2)
            it "returns correct chunking information from compressed SDS - 2" $ do
                sd_id              <- check =<< sd_start "test-data/sd/chktst.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 6
                chunkParams        <- check =<< sd_getchunkinfo sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                chunkParams `shouldBe` (HDFChunkParams [3,2] $ HDFCompDeflate 6)
            it "returns correct chunking information from compressed SDS - 3" $ do
                sd_id              <- check =<< sd_start "test-data/sd/chknbit.hdf" hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                chunkParams        <- check =<< sd_getchunkinfo sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                chunkParams `shouldBe` (HDFChunkParams [2,2] $ HDFCompNBit 0 0 0 6 7)
        context "SDsetchunk" $ do
            it "converts SDS to chunked SDS" $ do
                sd_id              <- check =<< sd_start "new_chunked_sds.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HInt32 (D 4 :| 8)
                _                  <- check =<< sd_setchunk sds_id (HDFChunkParams [4,1] HDFCompNone)
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                return ()
        context "SDreadchunk" $ do
            it "reads SDS chunk - 1" $ do
                let expectedData = VS.fromList ([11, 21, 12, 22, 13, 23] :: [Word16])
                sd_id              <- check =<< sd_start "test-data/sd/chktst.hdf" hdf_read
                (SomeSDS t sds_id) <- check =<< sd_select sd_id 0
                case t of
                    HWord16 -> do
                        sdsChunk <- check =<< sd_readchunk sds_id [0,0]
                        _        <- check =<< sd_endaccess sds_id
                        _        <- check =<< sd_end sd_id
                        sdsChunk `shouldBe` expectedData
                    _ -> expectationFailure "Unexpected SDS data type"
            it "reads SDS chunk - 2" $ do
                let expectedData = VS.fromList ([37, 47, 38, 48, 39, 49] :: [Word16])
                sd_id              <- check =<< sd_start "test-data/sd/chktst.hdf" hdf_read
                (SomeSDS t sds_id) <- check =<< sd_select sd_id 0
                case t of
                    HWord16 -> do
                        sdsChunk <- check =<< sd_readchunk sds_id [2,1]
                        _        <- check =<< sd_endaccess sds_id
                        _        <- check =<< sd_end sd_id
                        sdsChunk `shouldBe` expectedData
                    _ -> expectationFailure "Unexpected SDS data type"
            it "rejects wrong SDS chunk coordinates" $ do
                sd_id              <- check =<< sd_start "test-data/sd/chktst.hdf" hdf_read
                (SomeSDS t sds_id) <- check =<< sd_select sd_id 0
                case t of
                    HWord16 -> do
                        (status, _) <- sd_readchunk sds_id [999,999]
                        _           <- check =<< sd_endaccess sds_id
                        _           <- check =<< sd_end sd_id
                        status `shouldBe` (-1)
                    _ -> expectationFailure "Unexpected SDS data type"
        context "SDwritechunk" $ do
            it "writes data to a chunked SDS" $ do
                let expectedData = VS.fromList ([1, 2, 3, 4] :: [Int32])
                sd_id              <- check =<< sd_start "chunked_sds.hdf" hdf_create
                sds_id             <- check =<< sd_create sd_id "emptyDataSet" HInt32 (D 4 :| 8)
                _                  <- check =<< sd_setfillvalue sds_id 0
                _                  <- check =<< sd_setchunk sds_id (HDFChunkParams [4,1] HDFCompNone)
                _                  <- check =<< sd_writechunk sds_id [0,0] expectedData
                sdsChunk           <- check =<< sd_readchunk sds_id [0,0]
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                sdsChunk `shouldBe` expectedData
    context "Raw data information" $ do
        context "SDgetanndatainfo" $ do
            it "gets SD label raw offset and length" $ do
                let filePath = "test-data/sd/tdfanndg.hdf"
                sd_id              <- check =<< sd_start filePath hdf_read
                annOffsetLen       <- check =<< sd_getanndatainfo sd_id hdf_ann_file_label
                _                  <- check =<< sd_end sd_id
                let
                    (ann2Ofst,ann2Len) = annOffsetLen !! 0
                    (ann1Ofst,ann1Len) = annOffsetLen !! 1
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral ann1Ofst)
                ann1 <- BS.hGet hndl (fromIntegral ann1Len)
                hSeek hndl AbsoluteSeek (fromIntegral ann2Ofst)
                ann2 <- BS.hGet hndl (fromIntegral ann2Len)
                hClose hndl
                ann1 `shouldBe` "File Label #1"
                ann2 `shouldBe` "File Label #2"
            it "gets SD description raw offset and length" $ do
                let filePath = "test-data/sd/tdfanndg.hdf"
                sd_id              <- check =<< sd_start filePath hdf_read
                annOffsetLen       <- check =<< sd_getanndatainfo sd_id hdf_ann_file_desc
                _                  <- check =<< sd_end sd_id
                let
                    (ann2Ofst,ann2Len) = annOffsetLen !! 0
                    (ann1Ofst,ann1Len) = annOffsetLen !! 1
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral ann1Ofst)
                ann1 <- BS.hGet hndl (fromIntegral ann1Len)
                hSeek hndl AbsoluteSeek (fromIntegral ann2Ofst)
                ann2 <- BS.hGet hndl (fromIntegral ann2Len)
                hClose hndl
                ann1 `shouldBe` "File Descr #1: This is a file label, added\n       by the DFAN interface...**END SDS 1 DESCR**\n"
                ann2 `shouldBe` "File Descr #2: This is another file label added\n       by the DFAN API as well.**END SDS 2 DESCR**\n"
            it "gets SDS label raw offset and length" $ do
                let filePath = "test-data/sd/tdfanndg.hdf"
                sd_id              <- check =<< sd_start filePath hdf_read
                sds_index          <- check =<< sd_nametoindex sd_id "Data-Set-3"
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id sds_index
                annOffsetLen       <- check =<< sd_getanndatainfo sds_id hdf_ann_data_label
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                let (annOfst,annLen) = annOffsetLen !! 0
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral annOfst)
                ann <- BS.hGet hndl (fromIntegral annLen)
                hClose hndl
                ann `shouldBe` "Object label #1: sds"
            it "gets SDS description raw offset and length" $ do
                let filePath = "test-data/sd/tdfanndg.hdf"
                sd_id              <- check =<< sd_start filePath hdf_read
                sds_index          <- check =<< sd_nametoindex sd_id "Data-Set-3"
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id sds_index
                annOffsetLen       <- check =<< sd_getanndatainfo sds_id hdf_ann_data_desc
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                let (annOfst,annLen) = annOffsetLen !! 0
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral annOfst)
                ann <- BS.hGet hndl (fromIntegral annLen)
                hClose hndl
                ann `shouldBe` "Object Descr #1: 1 2 3 4 5 6 7 8 9 10 11 12 \n       13 14 15 16 17 18 19 20 **END SDS 1 DESCR**\n"
        context "SDgetattdatainfo" $ do
            it "gets SD attribute raw offset and length" $ do
                let filePath = "test-data/sd/test1.hdf"
                sd_id              <- check =<< sd_start filePath hdf_read
                annOffsetLen       <- check =<< sd_getattdatainfo sd_id 0
                _                  <- check =<< sd_end sd_id
                let (attrOfst,attrLen) = annOffsetLen
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral attrOfst)
                attr <- BS.hGet hndl (fromIntegral attrLen)
                hClose hndl
                attr `shouldBe` "globulator"
            it "gets SDS attribute raw offset and length" $ do
                let filePath = "test-data/sd/test1.hdf"
                sd_id              <- check =<< sd_start filePath hdf_read
                sds_index          <- check =<< sd_nametoindex sd_id "DataSetAlpha"
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id sds_index
                annOffsetLen       <- check =<< sd_getattdatainfo sds_id 3
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                let (attrOfst,attrLen) = annOffsetLen
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral attrOfst)
                attr <- BS.hGet hndl (fromIntegral attrLen)
                hClose hndl
                attr `shouldBe` "TheLabel"
            it "gets dimension attribute raw offset and length" $ do
                let filePath = "test-data/sd/test1.hdf"
                sd_id              <- check =<< sd_start filePath hdf_read
                sds_index          <- check =<< sd_nametoindex sd_id "DataSetGamma"
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id sds_index
                dim_id             <- check =<< sd_getdimid sds_id 0
                annOffsetLen       <- check =<< sd_getattdatainfo dim_id 1
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                let (attrOfst,attrLen) = annOffsetLen
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral attrOfst)
                attr <- BS.hGet hndl (fromIntegral attrLen)
                hClose hndl
                attr `shouldBe` "DimLabel"
        context "SDgetoldattdatainfo" $ do
            it "gets old style SDS attribute raw offset and length" $ do
                let filePath = "test-data/sd/tdfsdatts.hdf"
                sd_id              <- check =<< sd_start filePath hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                annOffsetLen       <- check =<< sd_getoldattdatainfo sds_id Nothing "long_name"
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                let (attrOfst,attrLen) = annOffsetLen
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral attrOfst)
                attr <- BS.hGet hndl (fromIntegral attrLen)
                hClose hndl
                attr `shouldBe` "Datalabel"
            it "gets old style dimension attribute raw offset and length" $ do
                let filePath = "test-data/sd/tdfsdatts.hdf"
                sd_id              <- check =<< sd_start filePath hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                dim_id             <- check =<< sd_getdimid sds_id 1
                annOffsetLen       <- check =<< sd_getoldattdatainfo sds_id (Just dim_id) "format"
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                let (attrOfst,attrLen) = annOffsetLen
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral attrOfst)
                attr <- BS.hGet hndl (fromIntegral attrLen)
                hClose hndl
                attr `shouldBe` "c_dim2_fmt"
            it "handles missing old style attribute" $ do
                let filePath = "test-data/sd/tdfsdatts.hdf"
                sd_id              <- check =<< sd_start filePath hdf_read
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id 0
                dim_id             <- check =<< sd_getdimid sds_id 0
                annOffsetLen       <- check =<< sd_getoldattdatainfo sds_id (Just dim_id) "long_name"
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                let (_,attrLen) = annOffsetLen -- Apparently, the offset is not zero even when the attribute is not defined
                attrLen `shouldBe` 0
        context "SDgetdatainfo" $ do
            it "gets SDS raw data offsets and lengths" $ do
                let filePath = "test-data/sd/test1.hdf"
                    expected =
                        "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
                        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
                        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
                        \\x00\x00\x00\x64\x00\x00\x00\x65\x00\x00\x00\x66\x00\x00\x00\x67\
                        \\x00\x00\x00\x68\x00\x00\x00\x69\x00\x00\x00\x00\x00\x00\x00\x00\
                        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
                        \\x00\x00\x00\x64\x00\x00\x00\x65\x00\x00\x00\x66\x00\x00\x00\x67\
                        \\x00\x00\x00\x68\x00\x00\x00\x69"
                sd_id              <- check =<< sd_start filePath hdf_read
                sds_index          <- check =<< sd_nametoindex sd_id "FIXED"
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id sds_index
                annOffsetLen       <- check =<< sd_getdatainfo sds_id [] 0
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                let (dataOfst,dataLen) = head annOffsetLen
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral dataOfst)
                rawData <- BS.hGet hndl (fromIntegral dataLen)
                hClose hndl
                rawData `shouldBe` expected
    context "Miscellaneous" $ do
        context "SDgetexternalinfo" $ do
            it "handles SDS without external file" $ do
                let filePath = "test-data/sd/test1.hdf"
                    expected = ("", (0, 0))
                sd_id              <- check =<< sd_start filePath hdf_read
                sds_index          <- check =<< sd_nametoindex sd_id "DataSetAlpha"
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id sds_index
                externFileInfo     <- check =<< sd_getexternalinfo sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                externFileInfo `shouldBe` expected
            it "handles SDS with external file" $ do
                let filePath = "test-data/sd/exttst.hdf"
                    expected = ("ExternalSDSexisting", (1600,560))
                    expectedData =
                        "\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06\
                        \\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\
                        \\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\
                        \\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\
                        \\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\
                        \\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\
                        \\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\
                        \\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\
                        \\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\
                        \\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\
                        \\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\
                        \\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\
                        \\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\
                        \\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\
                        \\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\
                        \\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\
                        \\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\
                        \\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\
                        \\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\
                        \\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\
                        \\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\
                        \\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\
                        \\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\
                        \\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\
                        \\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\
                        \\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\
                        \\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\
                        \\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\
                        \\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\
                        \\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\
                        \\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06\
                        \\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\
                        \\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\
                        \\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\
                        \\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a"
                sd_id              <- check =<< sd_start filePath hdf_read
                sds_index          <- check =<< sd_nametoindex sd_id "Dataset 2"
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id sds_index
                externFileInfo     <- check =<< sd_getexternalinfo sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                externFileInfo `shouldBe` expected
                let extFileName        = fst externFileInfo
                    (dataOfst,dataLen) = snd externFileInfo
                hndl <- openBinaryFile ("test-data/sd/" ++ extFileName) ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral dataOfst)
                rawData <- BS.hGet hndl (fromIntegral dataLen)
                hClose hndl
                rawData `shouldBe` expectedData
        context "SDsetexternalfile" $ do
            it "moves existing SDS to an external file" $ do
                let origFilePath     = "test-data/sd/test1.hdf"
                    filePath         = "test1.primary.hdf"
                    externalFilePath = "test1.external.hdf"
                    expected         = (externalFilePath, (0,120))
                    expectedData     =
                        "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
                        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
                        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
                        \\x00\x00\x00\x64\x00\x00\x00\x65\x00\x00\x00\x66\x00\x00\x00\x67\
                        \\x00\x00\x00\x68\x00\x00\x00\x69\x00\x00\x00\x00\x00\x00\x00\x00\
                        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
                        \\x00\x00\x00\x64\x00\x00\x00\x65\x00\x00\x00\x66\x00\x00\x00\x67\
                        \\x00\x00\x00\x68\x00\x00\x00\x69"
                copyFileWithMetadata origFilePath filePath
                sd_id              <- check =<< sd_start filePath hdf_write
                sds_index          <- check =<< sd_nametoindex sd_id "FIXED"
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id sds_index
                _                  <- check =<< sd_setexternalfile sds_id externalFilePath 0
                externFileInfo     <- check =<< sd_getexternalinfo sds_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                externFileInfo `shouldBe` expected
                let extFileName        = fst externFileInfo
                    (dataOfst,dataLen) = snd externFileInfo
                hndl <- openBinaryFile extFileName ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral dataOfst)
                rawData <- BS.hGet hndl (fromIntegral dataLen)
                hClose hndl
                rawData `shouldBe` expectedData
            it "creates new SDS in external file" $ do
                let filePath         = "external_sds.primary.hdf"
                    externalFilePath = "external_sds.external.hdf"
                    sdsData = VS.fromList ([4,5,6,7] :: [Word8] )
                sd_id              <- check =<< sd_start filePath hdf_create
                sds_id             <- check =<< sd_create sd_id "DataSet" HWord8 (D 2 :| 2)
                _                  <- check =<< sd_setexternalfile sds_id externalFilePath 0
                _                  <- check =<< sd_writedata sds_id (D 0:|0) (D 1:|1) (D 2:|2) sdsData
                v                  <- check =<< sd_readdata sds_id (D 0:|0) (D 1:|1) (D 2:|2)
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                v `shouldBe` sdsData
        context "SDisdimval_bwcomp" $ do
            it "detects backward incompatible dimension" $ do
                let filePath     = "test-data/sd/test1.hdf"
                sd_id              <- check =<< sd_start filePath hdf_read
                sds_index          <- check =<< sd_nametoindex sd_id "DataSetAlpha"
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id sds_index
                dim_id             <- check =<< sd_getdimid sds_id 0
                bwCompat           <- check =<< sd_isdimval_bwcomp dim_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                bwCompat `shouldBe` False
            it "detects backward compatible dimension" $ do
                let filePath     = "test-data/sd/test1.hdf"
                sd_id              <- check =<< sd_start filePath hdf_read
                sds_index          <- check =<< sd_nametoindex sd_id "dimval_1_compat"
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id sds_index
                dim_id             <- check =<< sd_getdimid sds_id 1
                bwCompat           <- check =<< sd_isdimval_bwcomp dim_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                bwCompat `shouldBe` True
        context "SDsetdimval_comp" $ do
            it "set backward compatible mode for dimension" $ do
                let origFilePath     = "test-data/sd/test1.hdf"
                    filePath         = "test1.compat.hdf"
                copyFileWithMetadata origFilePath filePath
                sd_id              <- check =<< sd_start filePath hdf_write
                sds_index          <- check =<< sd_nametoindex sd_id "DataSetAlpha"
                (SomeSDS _ sds_id) <- check =<< sd_select sd_id sds_index
                dim_id             <- check =<< sd_getdimid sds_id 0
                _                  <- check =<< sd_setdimval_comp dim_id True
                bwCompat           <- check =<< sd_isdimval_bwcomp dim_id
                _                  <- check =<< sd_endaccess sds_id
                _                  <- check =<< sd_end sd_id
                bwCompat `shouldBe` True
    context "Read and write" $ do
        context "SDreaddata" $ do
            it "reads data from SDS - 1" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS t (sds_id :: SDataSetId n a)) <-
                                      check =<< sd_select sd_id 0
                case t of
                    HFloat32 -> case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 2) of
                        Just Refl -> do
                            v <- check =<< sd_readdata sds_id (D 1:|1) (D 1:|1) (D 3:|3)
                            _ <- check =<< sd_endaccess sds_id
                            _ <- check =<< sd_end sd_id
                            v `shouldBe` (VS.fromList [0,1,2,3,4,5,6,7,8])
                        Nothing -> expectationFailure "Unexpected SDS rank"
                    _ -> expectationFailure "Unexpected dimension data type"
            it "reads data from SDS - 2" $ do
                sd_id              <- check =<< sd_start "test-data/sd/test1.hdf" hdf_read
                (SomeSDS t (sds_id :: SDataSetId n a)) <-
                                      check =<< sd_select sd_id 0
                case t of
                    HFloat32 -> case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 2) of
                        Just Refl -> do
                            v <- check =<< sd_readdata sds_id (D 1:|1) (D 2:|1) (D 2:|1)
                            _ <- check =<< sd_endaccess sds_id
                            _ <- check =<< sd_end sd_id
                            v `shouldBe` (VS.fromList [0,6])
                        Nothing -> expectationFailure "Unexpected SDS rank"
                    _ -> expectationFailure "Unexpected dimension data type"
        context "SDwritedata" $ do
            it "writes data to SDS - 1" $ do
                let sdsData = VS.fromList ([4,5,6,7] :: [Word8] )
                sd_id  <- check =<< sd_start "write_sds_1.hdf" hdf_create
                sds_id <- check =<< sd_create sd_id "DataSet" HWord8 (D 2 :| 2)
                _      <- check =<< sd_writedata sds_id (D 0 :| 0) (D 1 :| 1) (D 2 :| 2) sdsData
                v      <- check =<< sd_readdata sds_id (D 0 :| 0) (D 1 :| 1) (D 2 :| 2)
                _      <- check =<< sd_endaccess sds_id
                _      <- check =<< sd_end sd_id
                v `shouldBe` sdsData
            it "writes data to SDS - 2" $ do
                sd_id  <- check =<< sd_start "write_sds_2.hdf" hdf_create
                sds_id <- check =<< sd_create sd_id "DataSet" HWord8 (D 2 :| 2)
                _      <- check =<< sd_setfillvalue sds_id 5
                _      <- check =<< sd_writedata sds_id (D 0 :| 0) (D 1 :| 1) (D 1 :| 1) (VS.fromList [0])
                v      <- check =<< sd_readdata sds_id (D 0 :| 0) (D 1 :| 1) (D 2 :| 2)
                _      <- check =<< sd_endaccess sds_id
                _      <- check =<< sd_end sd_id
                v `shouldBe` (VS.fromList [0,5,5,5])
