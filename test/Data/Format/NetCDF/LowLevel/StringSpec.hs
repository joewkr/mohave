{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
module Data.Format.NetCDF.LowLevel.StringSpec(spec) where

import           Control.Monad (forM_)
import           Data.Proxy
import           Data.Type.Equality ((:~:)(Refl))
import qualified Data.Vector.Storable as VS
import           GHC.TypeNats
import           System.FilePath ((</>))
import           Test.Hspec

import           Data.Format.NetCDF.LowLevel
import           Data.Format.NetCDF.LowLevel.Dimension
import           Data.Format.NetCDF.LowLevel.File
import           Data.Format.NetCDF.LowLevel.String
import           Data.Format.NetCDF.LowLevel.Variable
import           Testing.Common

spec :: Spec
spec = do
    describe "string-specific functions" $ do
        context "variables" $ do
            it "correctly reads a string from vector" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "string_vector"
                case t of
                    SNCString -> case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 1) of
                        Just Refl -> do
                            nc_data <- checkNC =<< nc_get_var1 nc_id var (D 1)
                            _       <- checkNC =<< nc_close nc_id
                            nc_str  <-             fromNCString nc_data
                            _       <- checkNC =<< nc_free_string nc_data
                            nc_str `shouldBe` "two"
                        Nothing -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "nc_get_vara_string" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "string_vector"
                case t of
                    SNCString -> case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 1) of
                        Just Refl -> do
                            nc_data <- checkNC =<< nc_get_vara_string nc_id var (D 0) (D 2)
                            _       <- checkNC =<< nc_close nc_id
                            forM_ [(0, "one"), (1, "two")] $ \(p,str) -> do
                                nc_str  <- fromNCString (nc_data VS.! p)
                                nc_str `shouldBe` str
                        Nothing -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "nc_get_var1_string" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "string_matrix"
                case t of
                    SNCString -> case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 2) of
                        Just Refl -> do
                            nc_str  <- checkNC =<< nc_get_var1_string nc_id var (D 1 :| 1)
                            _       <- checkNC =<< nc_close nc_id
                            nc_str `shouldBe` "ee"
                        Nothing -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "nc_get_var_string" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "string_vector"
                case t of
                    SNCString -> case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 1) of
                        Just Refl -> do
                            nc_data <- checkNC =<< nc_get_var_string nc_id var
                            _       <- checkNC =<< nc_close nc_id
                            forM_ (zip [0..] ["one", "two", "three"]) $ \(p,str) -> do
                                nc_str  <- fromNCString (nc_data VS.! p)
                                nc_str `shouldBe` str
                        Nothing -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "nc_get_vars_string" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "string_vector"
                case t of
                    SNCString -> case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 1) of
                        Just Refl -> do
                            nc_data <- checkNC =<< nc_get_vars_string nc_id var (D 0) (D 2) (D 2)
                            _       <- checkNC =<< nc_close nc_id
                            forM_ [(0, "one"), (1, "three")] $ \(p,str) -> do
                                nc_str  <- fromNCString (nc_data VS.! p)
                                nc_str `shouldBe` str
                        Nothing -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "nc_get_string" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "just_string"
                case t of
                    SNCString -> case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 0) of
                        Just Refl -> do
                            nc_str  <- checkNC =<< nc_get_string nc_id var
                            _       <- checkNC =<< nc_close nc_id
                            nc_str `shouldBe` "some text"
                        Nothing -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "nc_put_string" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "str1_write.nc") NCNetCDF4 NCClobber
                var_id                 <- checkNC =<< nc_def_scalar_var nc_id "variable" NCString
                _                      <- checkNC =<< nc_put_string nc_id var_id "test string"
                v                      <- checkNC =<< nc_get_string nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` "test string"
            it "nc_put_var1_string" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "str2_write.nc") NCNetCDF4 NCClobber
                dim_id                 <- checkNC =<< nc_def_dim nc_id "string_dim" (Just 4)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" NCString (D dim_id)
                _                      <- checkNC =<< nc_put_var1_string nc_id var_id (D 0) "Lorem"
                _                      <- checkNC =<< nc_put_var1_string nc_id var_id (D 1) "ipsum"
                _                      <- checkNC =<< nc_put_var1_string nc_id var_id (D 2) "dolor"
                _                      <- checkNC =<< nc_put_var1_string nc_id var_id (D 3) "sit"
                nc_data                <- checkNC =<< nc_get_var_string nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                forM_ (zip [0..] ["Lorem", "ipsum", "dolor", "sit"]) $ \(p,str) -> do
                    nc_str  <- fromNCString (nc_data VS.! p)
                    nc_str `shouldBe` str
        context "attributes" $ do
            context "nc_get_scalar_string_att" $ do
                it "correctly reads a string attribute - NetCDF4" $ do
                    nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                    attrValue              <- checkNC =<< nc_get_scalar_string_att nc_id Nothing "summary"
                    _                      <- checkNC =<< nc_close nc_id
                    attrValue `shouldBe` "test NetCDF file"
                it "correctly reads a string attribute - nc3" $ do
                    nc_id                  <- checkNC =<< nc_open "test-data/nc/test4.nc" NCNoWrite
                    attrValue              <- checkNC =<< nc_get_scalar_string_att nc_id Nothing "summary"
                    _                      <- checkNC =<< nc_close nc_id
                    attrValue `shouldBe` "test NetCDF file--classic format"
                it "reports error on a wrong attribute type" $ do
                    pendingWith "Haskell equivalents for NetCDF error codes are not implemented"
            context "nc_put_scalar_string_att" $ do
                it "correctly sets a string attribute - NetCDF4" $ do
                    nc_id                  <- checkNC =<< nc_create (testOutputPath </> "str1_attr.nc") NCNetCDF4 NCClobber
                    var_id                 <- checkNC =<< nc_def_scalar_var nc_id "variable" NCInt
                    _                      <- checkNC =<< nc_put_scalar_string_att nc_id (Just var_id) "summary" "test"
                    attrValue              <- checkNC =<< nc_get_scalar_string_att nc_id (Just var_id) "summary"
                    _                      <- checkNC =<< nc_close nc_id
                    attrValue `shouldBe` "test"
                it "correctly sets a string attribute - nc3" $ do
                    nc_id                  <- checkNC =<< nc_create (testOutputPath </> "str2_attr.nc") NCClassic NCClobber
                    var_id                 <- checkNC =<< nc_def_scalar_var nc_id "variable" NCInt
                    _                      <- checkNC =<< nc_put_scalar_string_att nc_id (Just var_id) "summary" "test nc3"
                    _                      <- checkNC =<< nc_enddef nc_id
                    attrValue              <- checkNC =<< nc_get_scalar_string_att nc_id (Just var_id) "summary"
                    _                      <- checkNC =<< nc_close nc_id
                    attrValue `shouldBe` "test nc3"
            context "nc_get_string_att" $ do
                it "correctly reads a string attribute - NetCDF4" $ do
                    nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                    attrValue              <- checkNC =<< nc_get_string_att nc_id Nothing "summary"
                    _                      <- checkNC =<< nc_close nc_id
                    attrValue `shouldBe` ["test NetCDF file", "NetCDF4"]
                it "correctly reads a string attribute - nc3" $ do
                    nc_id                  <- checkNC =<< nc_open "test-data/nc/test4.nc" NCNoWrite
                    attrValue              <- checkNC =<< nc_get_string_att nc_id Nothing "summary"
                    _                      <- checkNC =<< nc_close nc_id
                    -- Classic format can not store multiple strings in a single attribute
                    attrValue `shouldBe` ["test NetCDF file--classic format"]
            context "nc_put_string_att" $ do
                it "correctly sets a string attribute - NetCDF4" $ do
                    nc_id                  <- checkNC =<< nc_create (testOutputPath </> "str3_attr.nc") NCNetCDF4 NCClobber
                    var_id                 <- checkNC =<< nc_def_scalar_var nc_id "variable" NCInt
                    _                      <- checkNC =<< nc_put_string_att nc_id (Just var_id) "summary" ["test", "nc4"]
                    attrValue              <- checkNC =<< nc_get_string_att nc_id (Just var_id) "summary"
                    _                      <- checkNC =<< nc_close nc_id
                    attrValue `shouldBe` ["test", "nc4"]
                it "correctly sets a string attribute - nc3" $ do
                    nc_id                  <- checkNC =<< nc_create (testOutputPath </> "str4_attr.nc") NCClassic NCClobber
                    var_id                 <- checkNC =<< nc_def_scalar_var nc_id "variable" NCInt
                    _                      <- checkNC =<< nc_put_string_att nc_id (Just var_id) "summary" ["test", "nc3"]
                    _                      <- checkNC =<< nc_enddef nc_id
                    attrValue              <- checkNC =<< nc_get_string_att nc_id (Just var_id) "summary"
                    _                      <- checkNC =<< nc_close nc_id
                    attrValue `shouldBe` ["testnc3"]
