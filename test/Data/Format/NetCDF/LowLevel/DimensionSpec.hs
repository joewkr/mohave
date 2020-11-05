module Data.Format.NetCDF.LowLevel.DimensionSpec(spec) where

import           Test.Hspec
import           System.FilePath ((</>))

import           Data.Format.NetCDF.LowLevel
import           Data.Format.NetCDF.LowLevel.File
import           Data.Format.NetCDF.LowLevel.Dimension
import           Testing.Common

spec :: Spec
spec = do
    describe "Dimensions" $ do
        it "correctly defines a new dimension" $ do
            nc_id                  <- checkNC =<< nc_create (testOutputPath </> "dim1.nc") NCNetCDF4 NCClobber
            _                      <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 10)
            _                      <- checkNC =<< nc_close nc_id
            return ()
        it "correctly defines an unlimited dimension" $ do
            nc_id                  <- checkNC =<< nc_create (testOutputPath </> "dim2.nc") NCNetCDF4 NCClobber
            dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" Nothing
            unlimId                <- checkNC =<< nc_inq_unlimdim nc_id
            _                      <- checkNC =<< nc_close nc_id
            (dim_id == unlimId) `shouldBe` True
        it "returns correct dimension information" $ do
            nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
            dim_id                 <- checkNC =<< nc_inq_dimid nc_id "dim1"
            (dimName, dimLen)      <- checkNC =<< nc_inq_dim nc_id dim_id
            _                      <- checkNC =<< nc_close nc_id
            dimName `shouldBe` "dim1"
            dimLen `shouldBe` 7
        it "returns correct dimension length" $ do
            nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
            dim_id                 <- checkNC =<< nc_inq_dimid nc_id "dim1"
            dimLen                 <- checkNC =<< nc_inq_dimlen nc_id dim_id
            _                      <- checkNC =<< nc_close nc_id
            dimLen `shouldBe` 7
        it "returns correct dimension name" $ do
            nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
            dim_id                 <- checkNC =<< nc_inq_dimid nc_id "dim1"
            dimName                <- checkNC =<< nc_inq_dimname nc_id dim_id
            _                      <- checkNC =<< nc_close nc_id
            dimName `shouldBe` "dim1"
        it "returns correct number of dimensions" $ do
            nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
            numDims                <- checkNC =<< nc_inq_ndims nc_id
            _                      <- checkNC =<< nc_close nc_id
            numDims `shouldBe` 4
        it "returns correct id for unlimited dimension" $ do
            nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
            unlimId1               <- checkNC =<< nc_inq_dimid nc_id "dim4"
            unlimId2               <- checkNC =<< nc_inq_unlimdim nc_id
            _                      <- checkNC =<< nc_close nc_id
            (unlimId1 == unlimId2) `shouldBe` True
        it "correctly renames dimension" $ do
            nc_id                  <- checkNC =<< nc_create (testOutputPath </> "dim_rename.nc") NCNetCDF4 NCClobber
            dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 10)
            dimName1               <- checkNC =<< nc_inq_dimname nc_id dim_id
            dimName1 `shouldBe` "nc_dimension"
            _                      <- checkNC =<< nc_rename_dim nc_id dim_id "dim1"
            dimName2               <- checkNC =<< nc_inq_dimname nc_id dim_id
            _                      <- checkNC =<< nc_close nc_id
            dimName2 `shouldBe` "dim1"