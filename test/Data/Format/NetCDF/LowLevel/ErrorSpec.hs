module Data.Format.NetCDF.LowLevel.ErrorSpec(spec) where

import           Test.Hspec

import           Data.Format.NetCDF.LowLevel
import           Data.Format.NetCDF.LowLevel.Error

spec :: Spec
spec = do
    describe "error reporting" $ do
        it "converts HDF error code to string" $ do
            -- Check messages for just a couple of errors
            nc_strerror NC_NOERR  `shouldReturn` "No error"
            nc_strerror NC2_ERR   `shouldReturn` "Unknown Error" -- no additional details about the v2 API errors
            nc_strerror NC_EBADID `shouldReturn` "NetCDF: Not a valid ID"
            nc_strerror NC_ENFILE `shouldReturn` "NetCDF: Too many files open"
            nc_strerror NC_EEXIST `shouldReturn` "NetCDF: File exists && NC_NOCLOBBER"
            nc_strerror NC_EINVAL `shouldReturn` "NetCDF: Invalid argument"
            nc_strerror NC_EPERM  `shouldReturn` "NetCDF: Write to read only"

            nc_strerror NC_EMPI   `shouldReturn` "NetCDF: MPI operation failed."

            nc_strerror NC_UNEXPECTED `shouldReturn` "mohave NetCDF: unexpected value was returned after a foreign call"
