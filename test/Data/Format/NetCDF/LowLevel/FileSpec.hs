module Data.Format.NetCDF.LowLevel.FileSpec(spec) where

import qualified Data.Vector.Storable as VS
import           Test.Hspec
import           System.FilePath ((</>))

import           Data.Format.NetCDF.LowLevel
import           Data.Format.NetCDF.LowLevel.Dimension
import           Data.Format.NetCDF.LowLevel.File
import           Data.Format.NetCDF.LowLevel.Variable
import           Testing.Common

spec :: Spec
spec = do
    describe "NetCDF File and Data IO" $ do
        it "opens existing NetCDF file" $ do
            nc_id                  <- checkNC =<< nc_open "test-data/nc/empty.nc" NCNoWrite
            _                      <- checkNC =<< nc_close nc_id
            return ()
        it "correctly handles missing NetCDF file" $ do
            (status_1, nc_id)      <-             nc_open "test-data/nc/NULL" NCNoWrite
            (status_2, _)          <-             nc_close nc_id
            status_1 `shouldNotBe` 0
            status_2 `shouldNotBe` 0
        it "correctly creates a new NetCDF file" $ do
            nc_id                  <- checkNC =<< nc_create (testOutputPath </> "empty.nc") NCNetCDF4 NCClobber
            _                      <- checkNC =<< nc_close nc_id
            return ()
        it "respects creation mode" $ do
            (status_1, nc_id)      <-             nc_create (testOutputPath </> "empty.nc") NCNetCDF4 NCNoClobber
            (status_2, _)          <-             nc_close nc_id
            status_1 `shouldNotBe` 0
            status_2 `shouldNotBe` 0
        it "correctly enters the define mode" $ do
            nc_id                  <- checkNC =<< nc_open (testOutputPath </> "empty.nc") NCWrite
            _                      <- checkNC =<< nc_redef nc_id
            _                      <- checkNC =<< nc_enddef nc_id
            _                      <- checkNC =<< nc_close nc_id
            return ()
        it "returns correct path to the opened file" $ do
            let path1 = "test-data/nc/empty.nc"
            nc_id                  <- checkNC =<< nc_open path1 NCNoWrite
            path2                  <- checkNC =<< nc_inq_path nc_id
            _                      <- checkNC =<< nc_close nc_id
            path2 `shouldBe` path1
        it "returns correct NetCDF format" $ do
            nc_id_1                <- checkNC =<< nc_create (testOutputPath </> "empty_nc4.nc") NCNetCDF4 NCClobber
            format_1               <- checkNC =<< nc_inq_format nc_id_1
            _                      <- checkNC =<< nc_close nc_id_1
            format_1 `shouldBe` NCNetCDF4
            nc_id_2                <- checkNC =<< nc_create (testOutputPath </> "empty_nc3.nc") NCClassic NCClobber
            format_2               <- checkNC =<< nc_inq_format nc_id_2
            _                      <- checkNC =<< nc_close nc_id_2
            format_2 `shouldBe` NCClassic
            nc_id_3                <- checkNC =<< nc_create (testOutputPath </> "empty_nc4_strict.nc") (NCClassicModel NCNetCDF4) NCClobber
            format_3               <- checkNC =<< nc_inq_format nc_id_3
            _                      <- checkNC =<< nc_close nc_id_3
            format_3 `shouldBe` (NCClassicModel NCNetCDF4)
        it "returns correct extended NetCDF format" $ do
            nc_id                  <- checkNC =<< nc_open "test-data/nc/empty.nc" (NCShare .|. NCNoWrite)
            (format, mode)         <- checkNC =<< nc_inq_format_extended nc_id
            _                      <- checkNC =<< nc_close nc_id
            format `shouldBe` NCFormatXNChdf5
            mode `shouldSatisfy` (queryOpenMode NCShare)
        it "correctly sets the fill mode" $ do
            nc_id                  <- checkNC =<< nc_create (testOutputPath </> "file1.nc") NCNetCDF4 NCClobber
            _                      <- checkNC =<< nc_set_fill nc_id NCFill
            dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 2)
            var_id                 <- checkNC =<< nc_def_var nc_id "variable" NCInt64 (D dim_id)
            _                      <- checkNC =<< nc_def_var_fill nc_id var_id (Just 100)
            v                      <- checkNC =<< nc_get_var nc_id var_id
            _                      <- checkNC =<< nc_close nc_id
            v `shouldBe` VS.fromList [100, 100]
