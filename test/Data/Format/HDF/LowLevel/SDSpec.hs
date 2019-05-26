module Data.Format.HDF.LowLevel.SDSpec(spec) where

import           Test.Hspec

import           Data.Format.HDF.LowLevel.C.Definitions
import           Data.Format.HDF.LowLevel.SD

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
            (create_status, sds_id) <- sd_create sd_id "emptyDataSet" hdf_float64 [1,2,3]
            (close_status, _) <- sd_end sd_id
            [open_status, close_status] `shouldNotContain`[-1]
            create_status `shouldNotBe` (-1)
        it "correctly selects existing SDS" $ do
            (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
            (select_status, sds_id) <- sd_select sd_id 0
            (endaccess_status, _) <- sd_endaccess sds_id
            (close_status, _) <- sd_end sd_id
            [open_status, close_status] `shouldNotContain`[-1]
            select_status `shouldNotBe` (-1)
            endaccess_status `shouldNotBe` (-1)
        it "correctly handles missing SDS" $ do
            (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
            (select_status, sds_id) <- sd_select sd_id 999
            (endaccess_status, _) <- sd_endaccess sds_id
            (close_status, _) <- sd_end sd_id
            [open_status, close_status] `shouldNotContain`[-1]
            select_status `shouldBe` (-1)
            endaccess_status `shouldBe` (-1)
    describe "SD General inquiry" $ do
        context "SDcheckempty" $ do
            it "empty SDS" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/emptySDSs.hdf" hdf_read
                (select_status, sds_id) <- sd_select sd_id 0
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
                (select_status, sds_id) <- sd_select sd_id 1
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
                (select_status, sds_id) <- sd_select sd_id 999
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
                (select_status, sds_id) <- sd_select sd_id 0
                (getnamelen_status, fileNameLength) <- sd_getnamelen sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getnamelen_status `shouldNotBe` (-1)
                fileNameLength `shouldBe` 12
            it "dimension name length" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, sds_id) <- sd_select sd_id 0
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
                (select_status, sds_id) <- sd_select sd_id 0
                (getinfo_status, sdsInfo) <- sd_getinfo sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getinfo_status `shouldNotBe` (-1)
                sdsInfo `shouldBe` (SDataSetInfoRaw
                    "DataSetAlpha"
                    2 [4,8] (unHDFDataType hdf_float32) 6)
            it "long name SDS" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/SDSlongname.hdf" hdf_read
                (select_status, sds_id) <- sd_select sd_id 0
                (getinfo_status, sdsInfo) <- sd_getinfo sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getinfo_status `shouldNotBe` (-1)
                sdsInfo `shouldBe` (SDataSetInfoRaw
                    "The name of this dataset is long, and it is used to test the new variable length name feature"
                    2 [10,10] (unHDFDataType hdf_int32) 0)
        context "SDget_maxopenfiles" $ do
            it "reports reasonable limits" $ do
                (get_maxopenfiles_status, limits@(curr_max, sys_limit)) <- sd_get_maxopenfiles
                get_maxopenfiles_status `shouldNotBe` (-1)
                curr_max `shouldSatisfy` (> 0)
                curr_max `shouldSatisfy` (<= sys_limit)
                sys_limit `shouldSatisfy` (> 0)
            it "reflects updated limits" $ do
                (reset_maxopenfiles_status, new_limit) <- sd_reset_maxopenfiles 128
                (get_maxopenfiles_status, limits@(curr_max, sys_limit)) <- sd_get_maxopenfiles
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
                (select_status, sds_id) <- sd_select sd_id 0
                (idtoref_status, _) <- sd_idtoref sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                idtoref_status `shouldNotBe` (-1)
        context "SDiscoordvar" $ do
            it "detects coordinate variable" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, sds_id) <- sd_select sd_id 2
                (iscoordvar_status, isCoordVar) <- sd_iscoordvar sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                iscoordvar_status `shouldNotBe` (-1)
                isCoordVar `shouldBe` True
            it "detects non-coordinate variable" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, sds_id) <- sd_select sd_id 0
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
                (select_status, sds_id) <- sd_select sd_id 7
                (isisrecord_status, isRecord) <- sd_isisrecord sds_id
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                isisrecord_status `shouldNotBe` (-1)
                isRecord `shouldBe` True
            it "detects variable withou unlimited dimension" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, sds_id) <- sd_select sd_id 0
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
                (select_status, sds_id) <- sd_select sd_id 0
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
                (select_status, sds_id) <- sd_select sd_id 0
                (getdimid_status, _) <- sd_getdimid sds_id 0
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldNotBe` (-1)
            it "reports error on missing dimension" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, sds_id) <- sd_select sd_id 0
                (getdimid_status, _) <- sd_getdimid sds_id 999
                (endaccess_status, _) <- sd_endaccess sds_id
                (close_status, _) <- sd_end sd_id
                [open_status, close_status] `shouldNotContain`[-1]
                [select_status, endaccess_status] `shouldNotContain`[-1]
                getdimid_status `shouldBe` (-1)
        context "SDdiminfo" $ do
            it "returns dimension information" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (select_status, sds_id) <- sd_select sd_id 0
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
                                    4 (unHDFDataType hdf_int32) 4)
            it "returns unlimited dimension information" $ do
                (open_status, sd_id) <- sd_start "test-data/sd/test1.hdf" hdf_read
                (nametoindex_status, sds_index) <- sd_nametoindex sd_id "dimval_1_compat"
                (select_status, sds_id) <- sd_select sd_id sds_index
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
                                    0 0 0)
        context "SDsetdimname" $ do
            it "sets new dimension name" $ do
                (open_status, sd_id) <- sd_start "empty_sds.hdf" hdf_create
                (create_status, sds_id) <- sd_create sd_id "emptyDataSet" hdf_float64 [1,2,3]
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
                                    1 0 0)
