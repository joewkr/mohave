{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Format.HDF.LowLevel.SDSpec(spec) where

import           Data.Int (Int32)
import           Data.Word (Word8, Word16)
import           Test.Hspec

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Format.HDF.LowLevel.C.Definitions
import           Data.Format.HDF.LowLevel.Definitions
import           Data.Format.HDF.LowLevel.SD
import qualified Data.Vector.Storable as VS
import           Foreign.Ptr (castPtr)
import           System.IO
import           System.Directory (copyFileWithMetadata)

spec :: Spec
spec = do
    describe "SD access routines" $ do
        it "opens existing HDF file" $ do
            (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
            (close_status, _) <- sd_end sd_id
            [open_status, close_status] `shouldNotContain`[-1]
        it "correctly handles missing HDF file" $ do
            (open_status, sd_id) <- sd_start "test-data/sd/NULL" hdf_read
            (close_status, _) <- sd_end sd_id
            open_status `shouldBe` (-1)
            close_status `shouldBe` (-1)
        it "correctly creates new SDS" $ do
            (open_status, sd_id) <- sd_start "empty_sds.hdf" hdf_create
            (create_status, _) <- sd_create sd_id "emptyDataSet" HFloat64 [1,2,3]
            (close_status, _) <- sd_end sd_id
            [open_status, close_status] `shouldNotContain`[-1]
            create_status `shouldNotBe` (-1)
        it "correctly selects existing SDS" $ do
            (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
            (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
            (endaccess_status, _) <- sd_endaccess sds_id
            (close_status, _) <- sd_end sd_id
            [open_status, close_status] `shouldNotContain`[-1]
            select_status `shouldNotBe` (-1)
            endaccess_status `shouldNotBe` (-1)
        it "correctly handles missing SDS" $ do
            (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
            (select_status, SomeSDS _ sds_id) <- sd_select sd_id 999
            (endaccess_status, _) <- sd_endaccess sds_id
            (close_status, _) <- sd_end sd_id
            [open_status, close_status] `shouldNotContain`[-1]
            select_status `shouldBe` (-1)
            endaccess_status `shouldBe` (-1)
    describe "SD General inquiry" $ do
        context "SDcheckempty" $ do
            it "empty SDS" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/emptySDSs.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (checkempty_status, is_empty) <- sd_checkempty sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                select_status `shouldNotBe` (-1)
                checkempty_status `shouldNotBe` (-1)
                endaccess_status `shouldNotBe` (-1)
                is_empty `shouldBe` True
            it "non-empty SDS" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/emptySDSs.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 1
                (checkempty_status, is_empty) <- sd_checkempty sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                select_status `shouldNotBe` (-1)
                checkempty_status `shouldNotBe` (-1)
                endaccess_status `shouldNotBe` (-1)
                is_empty `shouldBe` False
            it "incorrect SDS" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/emptySDSs.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 999
                (checkempty_status, is_empty) <- sd_checkempty sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                select_status `shouldBe` (-1)
                checkempty_status `shouldBe` (-1)
                endaccess_status `shouldBe` (-1)
                is_empty `shouldBe` True
        context "SDfileinfo" $ do
            it "test1.hdf" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (fileinfo_status, fileInfo) <- sd_fileinfo sd_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                fileinfo_status `shouldNotBe` (-1)
                fileInfo `shouldBe` (9,1)
            it "test2.hdf" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test2.hdf" hdf_read
                (fileinfo_status, fileInfo) <- sd_fileinfo sd_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                fileinfo_status `shouldNotBe` (-1)
                fileInfo `shouldBe` (1,0)
        context "SDgetnamelen" $ do
            it "file name length" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (getnamelen_status, fileNameLength) <- sd_getnamelen sd_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                getnamelen_status `shouldNotBe` (-1)
                fileNameLength `shouldBe` 22
            it "dataset name length" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getnamelen_status, fileNameLength) <- sd_getnamelen sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getnamelen_status `shouldNotBe` (-1)
                fileNameLength `shouldBe` 12
            it "dimension name length" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (getnamelen_status, fileNameLength) <- sd_getnamelen dim_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                getnamelen_status `shouldNotBe` (-1)
                fileNameLength `shouldBe` 5
        context "SDgetfilename" $ do
            it "gets file name of correctly opened dataset" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (getfilename_status, fileName) <- sd_getfilename sd_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                getfilename_status `shouldNotBe` (-1)
                fileName `shouldBe` "test-data/sd/test1.hdf"
        context "SDgetinfo" $ do
            it "regular SDS" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getinfo_status, sdsInfo) <- sd_getinfo sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getinfo_status `shouldNotBe` (-1)
                sdsInfo `shouldBe` (SDataSetInfoRaw
                    "DataSetAlpha"
                    2 [4,8] (HDFValue HFloat32 ()) 6)
            it "long name SDS" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/SDSlongname.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getinfo_status, sdsInfo) <- sd_getinfo sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getinfo_status `shouldNotBe` (-1)
                sdsInfo `shouldBe` (SDataSetInfoRaw
                    "The name of this dataset is long, and it is used to test the new variable length name feature"
                    2 [10,10] (HDFValue HInt32 ()) 0)
        context "SDget_maxopenfiles" $ do
            it "reports reasonable limits" $ do
                (get_maxopenfiles_status, (curr_max, sys_limit)) <- sd_get_maxopenfiles
                get_maxopenfiles_status `shouldNotBe` (-1)
                curr_max `shouldSatisfy` (> 0)
                curr_max `shouldSatisfy` (<= sys_limit)
                sys_limit `shouldSatisfy` (> 0)
            it "reflects updated limits" $ do
                (reset_maxopenfiles_status, new_limit) <- sd_reset_maxopenfiles 128
                (get_maxopenfiles_status, (curr_max, _)) <- sd_get_maxopenfiles
                [reset_maxopenfiles_status, get_maxopenfiles_status] `shouldNotContain`[-1]
                (curr_max == new_limit) `shouldBe` True
        context "SDget_numopenfiles" $ do
            it "no open files" $ do
                (get_numopenfiles_status, numFiles) <- sd_get_numopenfiles
                get_numopenfiles_status `shouldNotBe` (-1)
                numFiles `shouldBe` 0
            it "some open files" $ do
                (open_status_1, sd_id_1) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (open_status_2, sd_id_2) <- sd_start "test-data/sd/test2.hdf" hdf_read
                (get_numopenfiles_status, numFiles) <- sd_get_numopenfiles
                (close_status_1, _) <- sd_end sd_id_1
                (close_status_2, _) <- sd_end sd_id_2
                [open_status_1, close_status_1] `shouldNotContain`[-1]
                [open_status_2, close_status_2] `shouldNotContain`[-1]
                get_numopenfiles_status `shouldNotBe` (-1)
                numFiles `shouldBe` 2
        context "SDgetnumvars_byname" $ do
            it "reports multiple variables" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/vars_samename.hdf" hdf_read
                (getnumvars_byname_status, numVars) <- sd_getnumvars_byname sd_id "Common Name"
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                getnumvars_byname_status `shouldNotBe` (-1)
                numVars `shouldBe` 3
            it "reports unique variable" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (getnumvars_byname_status, numVars) <- sd_getnumvars_byname sd_id "DataSetAlpha"
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                getnumvars_byname_status `shouldNotBe` (-1)
                numVars `shouldBe` 1
        context "SDidtoref" $ do
            it "reports reference number for SDS" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (idtoref_status, _) <- sd_idtoref sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                idtoref_status `shouldNotBe` (-1)
        context "SDiscoordvar" $ do
            it "detects coordinate variable" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 2
                (iscoordvar_status, isCoordVar) <- sd_iscoordvar sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                iscoordvar_status `shouldNotBe` (-1)
                isCoordVar `shouldBe` True
            it "detects non-coordinate variable" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (iscoordvar_status, isCoordVar) <- sd_iscoordvar sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                iscoordvar_status `shouldNotBe` (-1)
                isCoordVar `shouldBe` False
        context "SDisrecord" $ do
            it "detects variable with unlimited dimension" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 7
                (isisrecord_status, isRecord) <- sd_isisrecord sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                isisrecord_status `shouldNotBe` (-1)
                isRecord `shouldBe` True
            it "detects variable without unlimited dimension" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (isisrecord_status, isRecord) <- sd_isisrecord sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                isisrecord_status `shouldNotBe` (-1)
                isRecord `shouldBe` False
        context "SDnametoindex" $ do
            it "reports index of coordinate variable" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "MyDim"
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                nametoindex_status `shouldNotBe` (-1)
                sds_index `shouldBe` 2
            it "reports index of non-coordinate variable" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "DataSetAlpha"
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                nametoindex_status `shouldNotBe` (-1)
                sds_index `shouldBe` 0
        context "SDnametoindices" $ do
            it "reports unique variable" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (nametoindices_status, hdfVarList) <- sd_nametoindices sd_id "DataSetAlpha"
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                nametoindices_status `shouldNotBe` (-1)
                hdfVarList `shouldBe` [HDFVarList 0 0]
            it "reports multiple variables" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/vars_samename.hdf" hdf_read
                (nametoindices_status, hdfVarList) <- sd_nametoindices sd_id "Common Name"
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                nametoindices_status `shouldNotBe` (-1)
                hdfVarList `shouldBe` [HDFVarList 0 0, HDFVarList 1 0, HDFVarList 3 1]
        context "SDreftoindex" $ do
            it "correctly converts reference number to SDS id" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (idtoref_status, sds_ref) <- sd_idtoref sds_id
                (reftoindex_status, sds_index_from_ref) <- sd_reftoindex sd_id sds_ref
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                idtoref_status `shouldNotBe` (-1)
                reftoindex_status `shouldNotBe` (-1)
                sds_index_from_ref `shouldBe` 0
        context "SDreset_maxopenfiles" $ do
            it "sets new limit for open hdf files" $ do
                (reset_maxopenfiles_status_1, old_limit) <- sd_reset_maxopenfiles 0
                (reset_maxopenfiles_status_2, _) <- sd_reset_maxopenfiles 128
                (reset_maxopenfiles_status_3, _) <- sd_reset_maxopenfiles old_limit
                [   reset_maxopenfiles_status_1
                  , reset_maxopenfiles_status_2
                  , reset_maxopenfiles_status_3 ] `shouldNotContain`[-1]
    describe "Dimensions" $ do
        context "SDgetdimid" $ do
            it "returns dimension id" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getdimid_status, _) <- sd_getdimid sds_id 0
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
            it "reports error on missing dimension" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getdimid_status, _) <- sd_getdimid sds_id 999
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldBe` (-1)
        context "SDdiminfo" $ do
            it "returns dimension information" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (diminfo_status, dimInfo) <- sd_diminfo dim_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                diminfo_status `shouldNotBe` (-1)
                dimInfo `shouldBe` (SDimensionInfoRaw
                                    "MyDim"
                                    4 (HDFValue HInt32 ()) 4)
            it "returns unlimited dimension information" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "dimval_1_compat"
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id sds_index
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (diminfo_status, dimInfo) <- sd_diminfo dim_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                nametoindex_status `shouldNotBe` (-1)
                getdimid_status `shouldNotBe` (-1)
                diminfo_status `shouldNotBe` (-1)
                dimInfo `shouldBe` (SDimensionInfoRaw
                                    "fakeDim8"
                                    0 (HDFValue HNone ()) 0)
        context "SDsetdimname" $ do
            it "sets new dimension name" $ do
                (open_status, sd_id) <- sd_start "empty_sds.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HFloat64 [1,2,3]
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (setdimname_status, _) <- sd_setdimname dim_id "MyDim"
                (diminfo_status, dimInfo) <- sd_diminfo dim_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                setdimname_status `shouldNotBe` (-1)
                diminfo_status `shouldNotBe` (-1)
                dimInfo `shouldBe` (SDimensionInfoRaw
                                    "MyDim"
                                    1 (HDFValue HNone ()) 0)
    context "Dimension scales" $ do
        context "SDsetdimscale" $ do
            it "sets dimension scale - 1" $ do
                (open_status, sd_id) <- sd_start "dimension_scale_1.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HFloat64 [5,2,3]
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (setdimname_status, _) <- sd_setdimname dim_id "MyDim"
                (setdimscale_status, _) <- sd_setdimscale dim_id HInt32 (VS.fromList [1,2,3,4,5])
                (diminfo_status, dimInfo) <- sd_diminfo dim_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                setdimname_status `shouldNotBe` (-1)
                setdimscale_status `shouldNotBe` (-1)
                diminfo_status `shouldNotBe` (-1)
                dimInfo `shouldBe` (SDimensionInfoRaw
                                    "MyDim"
                                    5 (HDFValue HInt32 ()) 0)
            it "sets dimension scale - 2" $ do
                let origDimScale = VS.fromList [5,4,3,2,1]
                (open_status, sd_id) <- sd_start "dimension_scale_2.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HFloat64 [5,2,3]
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (setdimname_status, _) <- sd_setdimname dim_id "MyDim"
                (setdimscale_status, _) <- sd_setdimscale dim_id HInt32 origDimScale
                (getdimscale_status, HDFValue t v) <- sd_getdimscale sds_id dim_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                setdimname_status `shouldNotBe` (-1)
                setdimscale_status `shouldNotBe` (-1)
                getdimscale_status `shouldNotBe` (-1)
                case t of
                    HInt32 -> v `shouldBe` origDimScale
                    _ -> expectationFailure "Unexpected dimension data type"
        context "SDgetdimscale" $ do
            it "gets dimension scale" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (getdimscale_status, HDFValue t v) <- sd_getdimscale sds_id dim_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                getdimscale_status `shouldNotBe` (-1)
                case t of
                    HInt32 -> v `shouldBe` (VS.fromList [1,5,7,24])
                    _ -> expectationFailure "Unexpected dimension data type"
            it "gets unlimited dimension scale" $ do
                let sdsData     = VS.fromList ([4,5,6,7] :: [Word8])
                    sdsDimScale = VS.fromList ([1,2,3,4] :: [Word16])
                (open_status, sd_id) <- sd_start "unlimited_dim_scale.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "DataSet" HWord8 [0]
                (writedata_status, _) <- sd_writedata sds_id [0] [1] [4] sdsData
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (setdimname_status, _) <- sd_setdimname dim_id "MyDim"
                (setdimscale_status, _) <- sd_setdimscale dim_id HWord16 sdsDimScale
                (getdimscale_status, HDFValue t v) <- sd_getdimscale sds_id dim_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                writedata_status `shouldNotBe` (-1)
                getdimid_status `shouldNotBe` (-1)
                setdimname_status `shouldNotBe` (-1)
                setdimscale_status `shouldNotBe` (-1)
                getdimscale_status `shouldNotBe` (-1)
                case t of
                    HWord16 -> v `shouldBe` sdsDimScale
                    _ -> expectationFailure "Unexpected dimension data type"
    context "User-defined attributes" $ do
        context "SDfindattr" $ do
            it "finds global attribute" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (findattr_status, attr_index) <- sd_findattr sd_id "F-attr"
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                findattr_status `shouldNotBe` (-1)
                attr_index `shouldBe` 0
            it "finds SDS attribute" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (findattr_status, attr_index) <- sd_findattr sds_id "units"
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                findattr_status `shouldNotBe` (-1)
                attr_index `shouldBe` 4
            it "finds dimension attribute" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (findattr_status, attr_index) <- sd_findattr dim_id "format"
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                findattr_status `shouldNotBe` (-1)
                attr_index `shouldBe` 2
        context "SDattrinfo" $ do
            it "returns global attribute information" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (findattr_status, attr_index) <- sd_findattr sd_id "F-attr"
                (attrinfo_status, attrInfo) <- sd_attrinfo sd_id attr_index
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                findattr_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "F-attr"
                                     10
                                     (HDFValue HChar8 ()))
            it "returns SDS attribute information" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (findattr_status, attr_index) <- sd_findattr sds_id "valid_range"
                (attrinfo_status, attrInfo) <- sd_attrinfo sds_id attr_index
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                findattr_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "valid_range"
                                     2
                                     (HDFValue HFloat32 ()))
            it "returns dimension attribute information" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (findattr_status, attr_index) <- sd_findattr dim_id "DimAttr"
                (attrinfo_status, attrInfo) <- sd_attrinfo dim_id attr_index
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                findattr_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "DimAttr"
                                     1
                                     (HDFValue HFloat32 ()))
        context "SDsetattr" $ do
            it "sets global attribute - ByteString" $ do
                let testAttr = BS8.pack "Test attribute - 1"
                (open_status, sd_id) <- sd_start "global_attr_1.hdf" hdf_create
                (setatt_status, _) <- sd_setattr sd_id "Test_1" HChar8 testAttr
                (attrinfo_status, attrInfo) <- sd_attrinfo sd_id 0
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                attrinfo_status `shouldNotBe` (-1)
                setatt_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_1"
                                     18
                                     (HDFValue HChar8 ()))
            it "sets global attribute - ByteString UChar" $ do
                let testAttr = BS8.pack "Test attribute - 1"
                (open_status, sd_id) <- sd_start "global_attr_2.hdf" hdf_create
                (setatt_status, _) <- sd_setattr sd_id "Test_1" HUChar8 testAttr
                (readattr_status, HDFValue t v) <- sd_readattr sd_id 0
                (attrinfo_status, attrInfo) <- sd_attrinfo sd_id 0
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                attrinfo_status `shouldNotBe` (-1)
                setatt_status `shouldNotBe` (-1)
                readattr_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
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
                (open_status, sd_id) <- sd_start "global_attr_3.hdf" hdf_create
                (setatt_status, _) <- sd_setattr sd_id "Test_2" HInt32 testAttr
                (attrinfo_status, attrInfo) <- sd_attrinfo sd_id 0
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                setatt_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_2"
                                     10
                                     (HDFValue HInt32 ()))
            it "sets global attribute - Vector" $ do
                let testAttr = VS.fromList ([-10..10] :: [Float])
                (open_status, sd_id) <- sd_start "global_attr_4.hdf" hdf_create
                (setatt_status, _) <- sd_setattr sd_id "Test_2" HFloat32 testAttr
                (attrinfo_status, attrInfo) <- sd_attrinfo sd_id 0
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                setatt_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_2"
                                     21
                                     (HDFValue HFloat32 ()))
            it "sets SDS attribute - ByteString" $ do
                let testAttr = BS8.pack "Test attribute - 1"
                (open_status, sd_id) <- sd_start "sds_attr_1.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HFloat64 [1,2,3]
                (setatt_status, _) <- sd_setattr sds_id "Test_1" HChar8 testAttr
                (attrinfo_status, attrInfo) <- sd_attrinfo sds_id 0
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                attrinfo_status `shouldNotBe` (-1)
                setatt_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_1"
                                     18
                                     (HDFValue HChar8 ()))
            it "sets SDS attribute - ByteString UChar" $ do
                let testAttr = BS8.pack "Test attribute - 1"
                (open_status, sd_id) <- sd_start "sds_attr_2.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HFloat64 [1,2,3]
                (setatt_status, _) <- sd_setattr sds_id "Test_1" HUChar8 testAttr
                (attrinfo_status, attrInfo) <- sd_attrinfo sds_id 0
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                attrinfo_status `shouldNotBe` (-1)
                setatt_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_1"
                                     18
                                     (HDFValue HUChar8 ()))
            it "sets SDS attribute - List" $ do
                let testAttr = [1..10] :: [Int32]
                (open_status, sd_id) <- sd_start "sds_attr_3.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HFloat64 [1,2,3]
                (setatt_status, _) <- sd_setattr sds_id "Test_2" HInt32 testAttr
                (attrinfo_status, attrInfo) <- sd_attrinfo sds_id 0
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                setatt_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_2"
                                     10
                                     (HDFValue HInt32 ()))
            it "sets SDS attribute - Vector" $ do
                let testAttr = VS.fromList ([-10..10] :: [Float])
                (open_status, sd_id) <- sd_start "sds_attr_4.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HFloat64 [1,2,3]
                (setatt_status, _) <- sd_setattr sds_id "Test_2" HFloat32 testAttr
                (attrinfo_status, attrInfo) <- sd_attrinfo sds_id 0
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                setatt_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_2"
                                     21
                                     (HDFValue HFloat32 ()))
            it "sets dimension attribute - ByteString" $ do
                let testAttr = BS8.pack "Test attribute - 1"
                (open_status, sd_id) <- sd_start "dim_attr_1.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HFloat64 [1,2,3]
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (setatt_status, _) <- sd_setattr dim_id "Test_1" HChar8 testAttr
                (attrinfo_status, attrInfo) <- sd_attrinfo dim_id 0
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
                setatt_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_1"
                                     18
                                     (HDFValue HChar8 ()))
            it "sets dimension attribute - ByteString UChar" $ do
                let testAttr = BS8.pack "Test attribute - 1"
                (open_status, sd_id) <- sd_start "dim_attr_2.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HFloat64 [1,2,3]
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (setatt_status, _) <- sd_setattr dim_id "Test_1" HUChar8 testAttr
                (attrinfo_status, attrInfo) <- sd_attrinfo dim_id 0
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
                setatt_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_1"
                                     18
                                     (HDFValue HUChar8 ()))
            it "sets dimension attribute - List" $ do
                let testAttr = [1..10] :: [Int32]
                (open_status, sd_id) <- sd_start "dim_attr_3.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HFloat64 [1,2,3]
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (setatt_status, _) <- sd_setattr dim_id "Test_2" HInt32 testAttr
                (attrinfo_status, attrInfo) <- sd_attrinfo dim_id 0
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                setatt_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_2"
                                     10
                                     (HDFValue HInt32 ()))
            it "sets dimension attribute - Vector" $ do
                let testAttr = VS.fromList ([-10..10] :: [Float])
                (open_status, sd_id) <- sd_start "dim_attr_4.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HFloat64 [1,2,3]
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (setatt_status, _) <- sd_setattr dim_id "Test_2" HFloat32 testAttr
                (attrinfo_status, attrInfo) <- sd_attrinfo dim_id 0
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                setatt_status `shouldNotBe` (-1)
                attrinfo_status `shouldNotBe` (-1)
                attrInfo `shouldBe` (SAttributeInfoRaw
                                     "Test_2"
                                     21
                                     (HDFValue HFloat32 ()))
        context "SDreadattr" $ do
            it "reads global attribute" $ do
                let expectedData = BS8.pack "globulator"
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (readattr_status, HDFValue t v) <- sd_readattr sd_id 0
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                readattr_status `shouldNotBe` (-1)
                case t of
                    HChar8 -> do
                        strAttr <- VS.unsafeWith v $ \vPtr -> BS.packCStringLen (castPtr vPtr, VS.length v)
                        strAttr `shouldBe` expectedData
                    _ -> expectationFailure "Unexpected dimension data type"
            it "reads SDS attribute" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (findattr_status, attr_index) <- sd_findattr sds_id "valid_range"
                (readattr_status, HDFValue t v) <- sd_readattr sds_id attr_index
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                findattr_status `shouldNotBe` (-1)
                readattr_status `shouldNotBe` (-1)
                case t of
                    HFloat32 -> v `shouldBe` (VS.fromList [4.5999999, 10.0])
                    _ -> expectationFailure "Unexpected dimension data type"
            it "reads dimension attribute" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (findattr_status, attr_index) <- sd_findattr dim_id "DimAttr"
                (readattr_status, HDFValue t v) <- sd_readattr dim_id attr_index
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                findattr_status `shouldNotBe` (-1)
                readattr_status `shouldNotBe` (-1)
                case t of
                    HFloat32 -> v `shouldBe` (VS.fromList [3.1415])
                    _ -> expectationFailure "Unexpected dimension data type"
    context "Predefined attributes" $ do
        context "SDgetcal" $ do
            it "reports calibration parameters" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 1
                (getcal_status, calibrationParams) <- sd_getcal sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getcal_status `shouldNotBe` (-1)
                calibrationParams `shouldBe`
                    (SCalibrationParametersRaw 1.0 5.0 3.0 2.5 (HDFValue HInt8 ()))
            it "handles missing calibration parameters" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getcal_status, _) <- sd_getcal sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getcal_status `shouldBe` (-1)
        context "SDgetdatastrs" $ do
            it "returns predefined string attributes" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/tdfsdatts.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getdatastrs_status, defaultStringAttrs) <- sd_getdatastrs sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getdatastrs_status `shouldNotBe` (-1)
                defaultStringAttrs `shouldBe`
                    (SDsetDescStringsRaw
                        "Datalabel"
                        "Dataunit"
                        "Datafmt"
                        "coordsys")
        context "SDgetdimstrs" $ do
            it "returns predefined string attributes" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/dim.hdf" hdf_read
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "HDF Data 2"
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id sds_index
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (getdimstrs_status, defaultStringAttrs) <- sd_getdimstrs dim_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                nametoindex_status `shouldNotBe` (-1)
                getdimid_status `shouldNotBe` (-1)
                getdimstrs_status `shouldNotBe` (-1)
                defaultStringAttrs `shouldBe`
                    (SDimDescStringsRaw
                        "DimLabel"
                        "Units"
                        "TheFormat")
        context "SDgetfillvalue" $ do
            it "returns correct Float fill value" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS t sds_id) <- sd_select sd_id 0
                case t of
                    HFloat32 -> do
                        (getfillvalue_status, fillValue) <- sd_getfillvalue sds_id
                        (endaccess_status, _) <- sd_endaccess sds_id
                        (close_status, _) <- sd_end sd_id
                        [open_status, close_status] `shouldNotContain`[-1]
                        [select_status, endaccess_status] `shouldNotContain`[-1]
                        getfillvalue_status `shouldNotBe` (-1)
                        fillValue `shouldBe` (-17.5 :: Float)
                    _ -> expectationFailure "Unexpected SDS data type"
            it "returns correct Int fill value" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "FIXED1"
                (select_status, SomeSDS t sds_id) <- sd_select sd_id sds_index
                case t of
                    HInt32 -> do
                        (getfillvalue_status, fillValue) <- sd_getfillvalue sds_id
                        (endaccess_status, _) <- sd_endaccess sds_id
                        (close_status, _) <- sd_end sd_id
                        [open_status, close_status] `shouldNotContain`[-1]
                        [select_status, endaccess_status] `shouldNotContain`[-1]
                        nametoindex_status `shouldNotBe` (-1)
                        getfillvalue_status `shouldNotBe` (-1)
                        fillValue `shouldBe` (-300 :: Int32)
                    _ -> expectationFailure "Unexpected SDS data type"
        context "SDgetrange" $ do
            it "returns correct range" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS t sds_id) <- sd_select sd_id 0
                case t of
                    HFloat32 -> do
                        (getrange_status, validRange) <- sd_getrange sds_id
                        (endaccess_status, _) <- sd_endaccess sds_id
                        (close_status, _) <- sd_end sd_id
                        [open_status, close_status] `shouldNotContain`[-1]
                        [select_status, endaccess_status] `shouldNotContain`[-1]
                        getrange_status `shouldNotBe` (-1)
                        validRange `shouldBe` ((4.6, 10.0) :: (Float, Float))
                    _ -> expectationFailure "Unexpected SDS data type"
        context "SDsetcal" $ do
            it "sets calibration parameters" $ do
                let calibrationParamsNew = SCalibrationParametersRaw 1.0 5.0 3.0 2.5 (HDFValue HFloat64 ())
                (open_status, sd_id) <- sd_start "empty_sds.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HWord8 [1,2,3]
                (setcal_status, _) <- sd_setcal sds_id calibrationParamsNew
                (getcal_status, calibrationParams) <- sd_getcal sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                setcal_status `shouldNotBe` (-1)
                getcal_status `shouldNotBe` (-1)
                calibrationParams `shouldBe` calibrationParamsNew
        context "SDsetdatastrs" $ do
            it "sets all predefined string attributes" $ do
                let defaultStringAttrsNew = SDsetDescStringsRaw "Label" "Unit" "Format" "Coordinate system"
                (open_status, sd_id) <- sd_start "empty_sds.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HWord8 [1,2,3]
                (setdatastrs_status, _) <- sd_setdatastrs sds_id defaultStringAttrsNew
                (getdatastrs_status, defaultStringAttrs) <- sd_getdatastrs sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                setdatastrs_status `shouldNotBe` (-1)
                getdatastrs_status `shouldNotBe` (-1)
                defaultStringAttrs `shouldBe` defaultStringAttrsNew
            it "sets some predefined string attributes" $ do
                let defaultStringAttrsNew = SDsetDescStringsRaw "Label" "Unit" "" ""
                (open_status, sd_id) <- sd_start "empty_sds.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HWord8 [1,2,3]
                (setdatastrs_status, _) <- sd_setdatastrs sds_id defaultStringAttrsNew
                (getdatastrs_status, defaultStringAttrs) <- sd_getdatastrs sds_id
                -- Check that we don't create empty attribute
                (findattr_status, _) <- sd_findattr sds_id "format"
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                setdatastrs_status `shouldNotBe` (-1)
                getdatastrs_status `shouldNotBe` (-1)
                findattr_status `shouldBe` (-1)
                defaultStringAttrs `shouldBe` defaultStringAttrsNew
        context "SDsetdimstrs" $ do
            it "sets all predefined string attributes" $ do
                let defaultStringAttrsNew = SDimDescStringsRaw "Label" "Unit" "Format"
                (open_status, sd_id) <- sd_start "empty_sds.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HWord8 [1,2,3]
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (setdimstrs_status, _) <- sd_setdimstrs dim_id defaultStringAttrsNew
                (getdimstrs_status, defaultStringAttrs) <- sd_getdimstrs dim_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                setdimstrs_status `shouldNotBe` (-1)
                getdimstrs_status `shouldNotBe` (-1)
                defaultStringAttrs `shouldBe` defaultStringAttrsNew
            it "sets some predefined string attributes" $ do
                let defaultStringAttrsNew = SDimDescStringsRaw "Label" "Unit" ""
                (open_status, sd_id) <- sd_start "empty_sds.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HWord8 [1,2,3]
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (setdimstrs_status, _) <- sd_setdimstrs dim_id defaultStringAttrsNew
                (getdimstrs_status, defaultStringAttrs) <- sd_getdimstrs dim_id
                -- Check that we don't create empty attribute
                (findattr_status, _) <- sd_findattr sds_id "format"
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                setdimstrs_status `shouldNotBe` (-1)
                getdimstrs_status `shouldNotBe` (-1)
                findattr_status `shouldBe` (-1)
                defaultStringAttrs `shouldBe` defaultStringAttrsNew
        context "SDsetfillvalue" $ do
            it "sets fill value" $ do
                (open_status, sd_id) <- sd_start "empty_sds.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HInt32 [1,2,3]
                (setfillvalue_status, _) <- sd_setfillvalue sds_id (-777 :: Int32)
                (getfillvalue_status, fillValue) <- sd_getfillvalue sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                setfillvalue_status `shouldNotBe` (-1)
                getfillvalue_status `shouldNotBe` (-1)
                fillValue `shouldBe` (-777 :: Int32)
        context "SDsetfillmode" $ do
            it "sets fill mode" $ do
                let sdsData = VS.fromList ([7] :: [Word8] )
                    expectedSdsData = VS.fromList ([5,7] :: [Word8] )
                (open_status, sd_id) <- sd_start "sds_fill_1.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "DataSet" HWord8 [2]
                (setfillmode_status, _) <- sd_setfillmode sd_id hdf_fill
                (setfillvalue_status, _) <- sd_setfillvalue sds_id 5
                (writedata_status, _) <- sd_writedata sds_id [1] [1] [1] sdsData
                (readdata_status, v) <- sd_readdata sds_id [0] [1] [2]
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                setfillmode_status `shouldNotBe` (-1)
                setfillvalue_status `shouldNotBe` (-1)
                writedata_status `shouldNotBe` (-1)
                readdata_status `shouldNotBe` (-1)
                v `shouldBe` expectedSdsData
            it "sets no-fill mode" $ do
                let sdsData = VS.fromList ([7] :: [Word8] )
                    expectedSdsData = VS.fromList ([5,7] :: [Word8] )
                (open_status, sd_id) <- sd_start "sds_nofill_1.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "DataSet" HWord8 [2]
                (setfillmode_status, _) <- sd_setfillmode sd_id hdf_nofill
                (setfillvalue_status, _) <- sd_setfillvalue sds_id 5
                (writedata_status, _) <- sd_writedata sds_id [1] [1] [1] sdsData
                (readdata_status, v) <- sd_readdata sds_id [0] [1] [2]
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                setfillmode_status `shouldNotBe` (-1)
                setfillvalue_status `shouldNotBe` (-1)
                writedata_status `shouldNotBe` (-1)
                readdata_status `shouldNotBe` (-1)
                v `shouldNotBe` expectedSdsData
        context "SDsetrange" $ do
            it "sets valid range for dataset" $ do
                (open_status, sd_id) <- sd_start "empty_sds.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HInt32 [1,2,3]
                (setrange_status, _) <- sd_setrange sds_id (0 :: Int32) (100 :: Int32)
                (getrange_status, validRange) <- sd_getrange sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                setrange_status `shouldNotBe` (-1)
                getrange_status `shouldNotBe` (-1)
                validRange `shouldBe` ((0, 100) :: (Int32, Int32))

    context "Compression" $ do
        context "SDsetcompress/SDgetcompinfo" $ do
            it "sets compression parameters" $ do
                let compParamsNew = HDFCompDeflate 9
                (open_status, sd_id) <- sd_start "empty_sds.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HInt32 [1,2,3]
                (setcompress_status, _) <- sd_setcompress sds_id compParamsNew
                (getcompinfo_status, compParams) <- sd_getcompinfo sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                setcompress_status `shouldNotBe` (-1)
                getcompinfo_status `shouldNotBe` (-1)
                compParams `shouldBe` compParamsNew
        context "SDsetnbitdataset" $ do
            it "sets n-bit compression parameters" $ do
                let compParamsNew = SDNBitCompParams 0 3 False False
                (open_status, sd_id) <- sd_start "empty_sds.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HInt32 [1,2,3]
                (setnbitdataset_status, _) <- sd_setnbitdataset sds_id compParamsNew
                (getcompinfo_status, compParams) <- sd_getcompinfo sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                setnbitdataset_status `shouldNotBe` (-1)
                getcompinfo_status `shouldNotBe` (-1)
                compParams `shouldBe` HDFCompNBit (fromHDataType HInt32) 0 0 0 3
    context "Chunking/Tiling" $ do
        context "SDgetchunkinfo" $ do
            it "handles empty SDS" $ do
                (open_status, sd_id) <- sd_start "empty_sds.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HInt32 [1,2,3]
                (getchunkinfo_status, chunkParams) <- sd_getchunkinfo sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                getchunkinfo_status `shouldNotBe` (-1)
                chunkParams `shouldBe` (HDFChunkParams [] HDFCompNone)
            it "returns correct chunking information" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/chktst.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getchunkinfo_status, chunkParams) <- sd_getchunkinfo sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getchunkinfo_status `shouldNotBe` (-1)
                chunkParams `shouldBe` (HDFChunkParams [3,2] HDFCompNone)
            it "returns correct chunking information from compressed SDS - 1" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/chktst.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 5
                (getchunkinfo_status, chunkParams) <- sd_getchunkinfo sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getchunkinfo_status `shouldNotBe` (-1)
                chunkParams `shouldBe` (HDFChunkParams [1,1,4] $ HDFCompSkHuff 2)
            it "returns correct chunking information from compressed SDS - 2" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/chktst.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 6
                (getchunkinfo_status, chunkParams) <- sd_getchunkinfo sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getchunkinfo_status `shouldNotBe` (-1)
                chunkParams `shouldBe` (HDFChunkParams [3,2] $ HDFCompDeflate 6)
            it "returns correct chunking information from compressed SDS - 3" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/chknbit.hdf" hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getchunkinfo_status, chunkParams) <- sd_getchunkinfo sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getchunkinfo_status `shouldNotBe` (-1)
                chunkParams `shouldBe` (HDFChunkParams [2,2] $ HDFCompNBit 0 0 0 6 7)
        context "SDsetchunk" $ do
            it "converts SDS to chunked SDS" $ do
                (open_status, sd_id) <- sd_start "new_chunked_sds.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HInt32 [4,8]
                (setchunk_status, _) <- sd_setchunk sds_id $ HDFChunkParams [4,1] HDFCompNone
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                setchunk_status `shouldNotBe` (-1)
        context "SDreadchunk" $ do
            it "reads SDS chunk - 1" $ do
                let expectedData = VS.fromList ([11, 21, 12, 22, 13, 23] :: [Word16])
                (open_status, sd_id) <- sd_start "test-data/sd/chktst.hdf" hdf_read
                (select_status, SomeSDS t sds_id) <- sd_select sd_id 0
                case t of
                    HWord16 -> do
                        (readchunk_status, sdsChunk) <- sd_readchunk sds_id [0,0]
                        (endaccess_status, _) <- sd_endaccess sds_id
                        (close_status, _) <- sd_end sd_id
                        [open_status, close_status] `shouldNotContain`[-1]
                        [select_status, endaccess_status] `shouldNotContain`[-1]
                        readchunk_status `shouldNotBe` (-1)
                        sdsChunk `shouldBe` expectedData
                    _ -> expectationFailure "Unexpected SDS data type"
            it "reads SDS chunk - 2" $ do
                let expectedData = VS.fromList ([37, 47, 38, 48, 39, 49] :: [Word16])
                (open_status, sd_id) <- sd_start "test-data/sd/chktst.hdf" hdf_read
                (select_status, SomeSDS t sds_id) <- sd_select sd_id 0
                case t of
                    HWord16 -> do
                        (readchunk_status, sdsChunk) <- sd_readchunk sds_id [2,1]
                        (endaccess_status, _) <- sd_endaccess sds_id
                        (close_status, _) <- sd_end sd_id
                        [open_status, close_status] `shouldNotContain`[-1]
                        [select_status, endaccess_status] `shouldNotContain`[-1]
                        readchunk_status `shouldNotBe` (-1)
                        sdsChunk `shouldBe` expectedData
                    _ -> expectationFailure "Unexpected SDS data type"
            it "rejects wrong SDS chunk coordinates" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/chktst.hdf" hdf_read
                (select_status, SomeSDS t sds_id) <- sd_select sd_id 0
                case t of
                    HWord16 -> do
                        (readchunk_status, _) <- sd_readchunk sds_id [999,999]
                        (endaccess_status, _) <- sd_endaccess sds_id
                        (close_status, _) <- sd_end sd_id
                        [open_status, close_status] `shouldNotContain`[-1]
                        [select_status, endaccess_status] `shouldNotContain`[-1]
                        readchunk_status `shouldBe` (-1)
                    _ -> expectationFailure "Unexpected SDS data type"
        context "SDwritechunk" $ do
            it "writes data to a chunked SDS" $ do
                let expectedData = VS.fromList ([1, 2, 3, 4] :: [Int32])
                (open_status, sd_id) <- sd_start "chunked_sds.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" HInt32 [4,8]
                (setfillvalue_status, _) <- sd_setfillvalue sds_id 0
                (setchunk_status, _) <- sd_setchunk sds_id $ HDFChunkParams [4,1] HDFCompNone
                (writechunk_status, _) <- sd_writechunk sds_id [0,0] expectedData
                (readchunk_status, sdsChunk) <- sd_readchunk sds_id [0,0]
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                setfillvalue_status `shouldNotBe` (-1)
                setchunk_status `shouldNotBe` (-1)
                writechunk_status `shouldNotBe` (-1)
                readchunk_status `shouldNotBe` (-1)
                sdsChunk `shouldBe` expectedData
    context "Raw data information" $ do
        context "SDgetanndatainfo" $ do
            it "gets SD label raw offset and length" $ do
                let filePath = "test-data/sd/tdfanndg.hdf"
                (open_status, sd_id) <- sd_start filePath hdf_read
                (getanndatainfo_status, annOffsetLen) <- sd_getanndatainfo sd_id hdf_ann_file_label
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                getanndatainfo_status `shouldNotBe` (-1)
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
                (open_status, sd_id) <- sd_start filePath hdf_read
                (getanndatainfo_status, annOffsetLen) <- sd_getanndatainfo sd_id hdf_ann_file_desc
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                getanndatainfo_status `shouldNotBe` (-1)
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
                (open_status, sd_id) <- sd_start filePath hdf_read
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "Data-Set-3"
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id sds_index
                (getanndatainfo_status, annOffsetLen) <- sd_getanndatainfo sds_id hdf_ann_data_label
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                nametoindex_status `shouldNotBe` (-1)
                getanndatainfo_status `shouldNotBe` (-1)
                let (annOfst,annLen) = annOffsetLen !! 0
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral annOfst)
                ann <- BS.hGet hndl (fromIntegral annLen)
                hClose hndl
                ann `shouldBe` "Object label #1: sds"
            it "gets SDS description raw offset and length" $ do
                let filePath = "test-data/sd/tdfanndg.hdf"
                (open_status, sd_id) <- sd_start filePath hdf_read
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "Data-Set-3"
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id sds_index
                (getanndatainfo_status, annOffsetLen) <- sd_getanndatainfo sds_id hdf_ann_data_desc
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                nametoindex_status `shouldNotBe` (-1)
                getanndatainfo_status `shouldNotBe` (-1)
                let (annOfst,annLen) = annOffsetLen !! 0
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral annOfst)
                ann <- BS.hGet hndl (fromIntegral annLen)
                hClose hndl
                ann `shouldBe` "Object Descr #1: 1 2 3 4 5 6 7 8 9 10 11 12 \n       13 14 15 16 17 18 19 20 **END SDS 1 DESCR**\n"
        context "SDgetattdatainfo" $ do
            it "gets SD attribute raw offset and length" $ do
                let filePath = "test-data/sd/test1.hdf"
                (open_status, sd_id) <- sd_start filePath hdf_read
                (getattdatainfo_status, annOffsetLen) <- sd_getattdatainfo sd_id 0
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                getattdatainfo_status `shouldNotBe` (-1)
                let (attrOfst,attrLen) = annOffsetLen
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral attrOfst)
                attr <- BS.hGet hndl (fromIntegral attrLen)
                hClose hndl
                attr `shouldBe` "globulator"
            it "gets SDS attribute raw offset and length" $ do
                let filePath = "test-data/sd/test1.hdf"
                (open_status, sd_id) <- sd_start filePath hdf_read
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "DataSetAlpha"
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id sds_index
                (getattdatainfo_status, annOffsetLen) <- sd_getattdatainfo sds_id 3
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                nametoindex_status `shouldNotBe` (-1)
                getattdatainfo_status `shouldNotBe` (-1)
                let (attrOfst,attrLen) = annOffsetLen
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral attrOfst)
                attr <- BS.hGet hndl (fromIntegral attrLen)
                hClose hndl
                attr `shouldBe` "TheLabel"
            it "gets dimension attribute raw offset and length" $ do
                let filePath = "test-data/sd/test1.hdf"
                (open_status, sd_id) <- sd_start filePath hdf_read
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "DataSetGamma"
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id sds_index
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (getattdatainfo_status, annOffsetLen) <- sd_getattdatainfo dim_id 1
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                nametoindex_status `shouldNotBe` (-1)
                getdimid_status `shouldNotBe` (-1)
                getattdatainfo_status `shouldNotBe` (-1)
                let (attrOfst,attrLen) = annOffsetLen
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral attrOfst)
                attr <- BS.hGet hndl (fromIntegral attrLen)
                hClose hndl
                attr `shouldBe` "DimLabel"
        context "SDgetoldattdatainfo" $ do
            it "gets old style SDS attribute raw offset and length" $ do
                let filePath = "test-data/sd/tdfsdatts.hdf"
                (open_status, sd_id) <- sd_start filePath hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getoldattdatainfo_status, annOffsetLen) <- sd_getoldattdatainfo sds_id Nothing "long_name"
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getoldattdatainfo_status `shouldNotBe` (-1)
                let (attrOfst,attrLen) = annOffsetLen
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral attrOfst)
                attr <- BS.hGet hndl (fromIntegral attrLen)
                hClose hndl
                attr `shouldBe` "Datalabel"
            it "gets old style dimension attribute raw offset and length" $ do
                let filePath = "test-data/sd/tdfsdatts.hdf"
                (open_status, sd_id) <- sd_start filePath hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getdimid_status, dim_id) <- sd_getdimid sds_id 1
                (getoldattdatainfo_status, annOffsetLen) <- sd_getoldattdatainfo sds_id (Just dim_id) "format"
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                getoldattdatainfo_status `shouldNotBe` (-1)
                let (attrOfst,attrLen) = annOffsetLen
                hndl <- openBinaryFile filePath ReadMode
                hSeek hndl AbsoluteSeek (fromIntegral attrOfst)
                attr <- BS.hGet hndl (fromIntegral attrLen)
                hClose hndl
                attr `shouldBe` "c_dim2_fmt"
            it "handles missing old style attribute" $ do
                let filePath = "test-data/sd/tdfsdatts.hdf"
                (open_status, sd_id) <- sd_start filePath hdf_read
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id 0
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (getoldattdatainfo_status, annOffsetLen) <- sd_getoldattdatainfo sds_id (Just dim_id) "long_name"
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
                getoldattdatainfo_status `shouldNotBe` (-1)
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
                (open_status, sd_id) <- sd_start filePath hdf_read
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "FIXED"
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id sds_index
                (getdatainfo_status, annOffsetLen) <- sd_getdatainfo sds_id [] 0
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                nametoindex_status `shouldNotBe` (-1)
                getdatainfo_status `shouldNotBe` (-1)
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
                (open_status, sd_id) <- sd_start filePath hdf_read
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "DataSetAlpha"
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id sds_index
                (getexternalinfo_status, externFileInfo) <- sd_getexternalinfo sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                nametoindex_status `shouldNotBe` (-1)
                getexternalinfo_status `shouldNotBe` (-1)
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
                (open_status, sd_id) <- sd_start filePath hdf_read
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "Dataset 2"
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id sds_index
                (getexternalinfo_status, externFileInfo) <- sd_getexternalinfo sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                nametoindex_status `shouldNotBe` (-1)
                getexternalinfo_status `shouldNotBe` (-1)
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
                (open_status, sd_id) <- sd_start filePath hdf_write
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "FIXED"
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id sds_index
                (setexternalfile_status, _) <- sd_setexternalfile sds_id externalFilePath 0
                (getexternalfile_status, externFileInfo) <- sd_getexternalinfo sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                nametoindex_status `shouldNotBe` (-1)
                setexternalfile_status `shouldNotBe` (-1)
                getexternalfile_status `shouldNotBe` (-1)
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
                (open_status, sd_id) <- sd_start filePath hdf_create
                (create_status, sds_id) <- sd_create sd_id "DataSet" HWord8 [2,2]
                (setexternalfile_status, _) <- sd_setexternalfile sds_id externalFilePath 0
                (writedata_status, _) <- sd_writedata sds_id [0,0] [1,1] [2,2] sdsData
                (readdata_status, v) <- sd_readdata sds_id [0,0] [1,1] [2,2]
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                setexternalfile_status `shouldNotBe` (-1)
                writedata_status `shouldNotBe` (-1)
                readdata_status `shouldNotBe` (-1)
                v `shouldBe` sdsData
        context "SDisdimval_bwcomp" $ do
            it "detects backward incompatible dimension" $ do
                let filePath     = "test-data/sd/test1.hdf"
                (open_status, sd_id) <- sd_start filePath hdf_read
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "DataSetAlpha"
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id sds_index
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (isdimval_bwcomp_status, bwCompat) <- sd_isdimval_bwcomp dim_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                nametoindex_status `shouldNotBe` (-1)
                getdimid_status `shouldNotBe` (-1)
                isdimval_bwcomp_status `shouldNotBe` (-1)
                bwCompat `shouldBe` False
            it "detects backward compatible dimension" $ do
                let filePath     = "test-data/sd/test1.hdf"
                (open_status, sd_id) <- sd_start filePath hdf_read
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "dimval_1_compat"
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id sds_index
                (getdimid_status, dim_id) <- sd_getdimid sds_id 1
                (isdimval_bwcomp_status, bwCompat) <- sd_isdimval_bwcomp dim_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                nametoindex_status `shouldNotBe` (-1)
                getdimid_status `shouldNotBe` (-1)
                isdimval_bwcomp_status `shouldNotBe` (-1)
                bwCompat `shouldBe` True
        context "SDsetdimval_comp" $ do
            it "set backward compatible mode for dimension" $ do
                let origFilePath     = "test-data/sd/test1.hdf"
                    filePath         = "test1.compat.hdf"
                copyFileWithMetadata origFilePath filePath
                (open_status, sd_id) <- sd_start filePath hdf_write
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "DataSetAlpha"
                (select_status, SomeSDS _ sds_id) <- sd_select sd_id sds_index
                (getdimid_status, dim_id) <- sd_getdimid sds_id 0
                (setdimval_comp_status, _) <- sd_setdimval_comp dim_id True
                (isdimval_bwcomp_status, bwCompat) <- sd_isdimval_bwcomp dim_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                nametoindex_status `shouldNotBe` (-1)
                getdimid_status `shouldNotBe` (-1)
                setdimval_comp_status `shouldNotBe` (-1)
                isdimval_bwcomp_status `shouldNotBe` (-1)
                bwCompat `shouldBe` True
        context "SDreaddata" $ do
            it "reads data from SDS - 1" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS t sds_id) <- sd_select sd_id 0
                case t of
                    HFloat32 -> do
                        (readdata_status, v) <- sd_readdata sds_id [1,1] [1,1] [3,3]
                        (endaccess_status, _) <- sd_endaccess sds_id
                        (close_status, _) <- sd_end sd_id
                        [open_status, close_status] `shouldNotContain`[-1]
                        [select_status, endaccess_status] `shouldNotContain`[-1]
                        readdata_status `shouldNotBe` (-1)
                        v `shouldBe` (VS.fromList [0,1,2,3,4,5,6,7,8])
                    _ -> expectationFailure "Unexpected dimension data type"
            it "reads data from SDS - 2" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, SomeSDS t sds_id) <- sd_select sd_id 0
                case t of
                    HFloat32 -> do
                        (readdata_status, v) <- sd_readdata sds_id [1,1] [2,1] [2,1]
                        (endaccess_status, _) <- sd_endaccess sds_id
                        (close_status, _) <- sd_end sd_id
                        [open_status, close_status] `shouldNotContain`[-1]
                        [select_status, endaccess_status] `shouldNotContain`[-1]
                        readdata_status `shouldNotBe` (-1)
                        v `shouldBe` (VS.fromList [0,6])
                    _ -> expectationFailure "Unexpected dimension data type"
        context "SDwritedata" $ do
            it "writes data to SDS - 1" $ do
                let sdsData = VS.fromList ([4,5,6,7] :: [Word8] )
                (open_status, sd_id) <- sd_start "write_sds_1.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "DataSet" HWord8 [2,2]
                (writedata_status, _) <- sd_writedata sds_id [0,0] [1,1] [2,2] sdsData
                (readdata_status, v) <- sd_readdata sds_id [0,0] [1,1] [2,2]
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                writedata_status `shouldNotBe` (-1)
                readdata_status `shouldNotBe` (-1)
                v `shouldBe` sdsData
            it "writes data to SDS - 2" $ do
                (open_status, sd_id) <- sd_start "write_sds_2.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "DataSet" HWord8 [2,2]
                (setfillvalue_status, _) <- sd_setfillvalue sds_id 5
                (writedata_status, _) <- sd_writedata sds_id [0,0] [1,1] [1,1] (VS.fromList [0])
                (readdata_status, v) <- sd_readdata sds_id [0,0] [1,1] [2,2]
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [create_status, endaccess_status] `shouldNotContain`[-1]
                setfillvalue_status `shouldNotBe` (-1)
                writedata_status `shouldNotBe` (-1)
                readdata_status `shouldNotBe` (-1)
                v `shouldBe` (VS.fromList [0,5,5,5])
