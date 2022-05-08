module Data.Format.NetCDF.LowLevel.ChunkCacheSpec(spec) where

import           Test.Hspec

import           Data.Format.NetCDF.LowLevel
import           Data.Format.NetCDF.LowLevel.File
import           Data.Format.NetCDF.LowLevel.ChunkCache
import           Data.Format.NetCDF.LowLevel.Variable
import           Testing.Common

spec :: Spec
spec = do
    describe "ChunkCache" $ do
        context "global chunk cache" $ do
            it "correctly sets global chunk cache" $ do
                let cacheParams = NCChunkCacheParams 1024 971 0.5
                _                       <- checkNC =<< nc_set_chunk_cache cacheParams
                newP                    <- checkNC =<< nc_get_chunk_cache
                newP `shouldBe` cacheParams
        context "per-variable chunk cache" $ do
            it "correctly sets per-variable chunk cache" $ do
                let cacheParams = NCChunkCacheParams 2048 971 0.25
                nc_id <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _  var) <- checkNC =<< nc_inq_varid nc_id "vector_var"
                _                       <- checkNC =<< nc_set_var_chunk_cache nc_id var cacheParams
                newP                    <- checkNC =<< nc_get_var_chunk_cache nc_id var
                _                       <- checkNC =<< nc_close nc_id
                newP `shouldBe` cacheParams

