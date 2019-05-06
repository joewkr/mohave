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