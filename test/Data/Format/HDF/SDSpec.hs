{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Format.HDF.SDSpec(spec) where

import           Data.Format.HDF.LowLevel.Definitions
import           Data.Format.HDF.SD
import           Test.Hspec

spec :: Spec
spec = do
    describe "SD bracket" $ do
        it "opens existing SD" $ do
            r <- withExistingSDHDF "test-data/sd/test1.hdf" $ \(_ :: SDFile 'HDFRead s) -> return ()
            r `shouldBe` Right ()
        it "handles missing SD" $ do
            r <- withExistingSDHDF "test-data/sd/NULL" $ \(_ :: SDFile 'HDFRead s) -> return ()
            r `shouldBe` Left DFE_BADNAME
    describe "SDS bracket" $ do
        it "starts access session for existing SDS" $ do
            r <- withExistingSDHDF "test-data/sd/test1.hdf" $ \(sd :: SDFile 'HDFRead s) ->
                withExistingSDS sd "DataSetAlpha" $ \_ -> return ()
            r `shouldBe` Right ()
        it "handles missing SDS" $ do
            r <- withExistingSDHDF "test-data/sd/test1.hdf" $ \(sd :: SDFile 'HDFRead s) ->
                withExistingSDS sd "NULL" $ \_ -> return ()
            r `shouldBe` Left DFE_SDS_NOTFOUND
