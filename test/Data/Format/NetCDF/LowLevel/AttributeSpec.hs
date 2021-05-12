{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Format.NetCDF.LowLevel.AttributeSpec(spec) where

import           Control.Monad (forM_)
import qualified Data.Vector.Storable as VS
import           Test.Hspec
import           System.FilePath ((</>))

import           Data.Format.NetCDF.LowLevel
import           Data.Format.NetCDF.LowLevel.Attribute
import           Data.Format.NetCDF.LowLevel.File
import           Data.Format.NetCDF.LowLevel.String
import           Data.Format.NetCDF.LowLevel.Variable
import           Testing.Common

spec :: Spec
spec = do
    describe "Attributes" $ do
        context "nc_inq_att" $ do
            it "returns correct attribute info" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "scalar_int"
                attrInfo               <- checkNC =<< nc_inq_att nc_id (Just var) "many_ints"
                _                      <- checkNC =<< nc_close nc_id
                attrInfo `shouldBe` (NCAttributeInfoRaw "many_ints" 3 (TypedValue NCInt64 ()))
        context "nc_inq_attid" $ do
            it "returns correct attribute number" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "string_vector"
                attrId                 <- checkNC =<< nc_inq_attid nc_id (Just var) "long_name"
                _                      <- checkNC =<< nc_close nc_id
                attrId `shouldBe` 0
            it "returns correct global attribute number" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                attrId                 <- checkNC =<< nc_inq_attid nc_id Nothing "version"
                _                      <- checkNC =<< nc_close nc_id
                attrId `shouldBe` 1
        context "nc_inq_attname" $ do
            it "returns correct attribute name" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "string_vector"
                attrName               <- checkNC =<< nc_inq_attname nc_id (Just var) 1
                _                      <- checkNC =<< nc_close nc_id
                attrName `shouldBe` "test_attr"
            it "returns correct global attribute name" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                attrName               <- checkNC =<< nc_inq_attname nc_id Nothing 0
                _                      <- checkNC =<< nc_close nc_id
                attrName `shouldBe` "summary"
        context "nc_inq_natts" $ do
            it "returns correct number of global attributes" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                numAttr                <- checkNC =<< nc_inq_natts nc_id
                _                      <- checkNC =<< nc_close nc_id
                numAttr `shouldBe` 2
        context "nc_inq_atttype" $ do
            it "returns correct attribute type" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "string_vector"
                attrType               <- checkNC =<< nc_inq_atttype nc_id (Just var) "test_attr"
                _                      <- checkNC =<< nc_close nc_id
                attrType `shouldBe` (TypedValue NCDouble ())
            it "returns correct global attribute type" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                attrType               <- checkNC =<< nc_inq_atttype nc_id Nothing "version"
                _                      <- checkNC =<< nc_close nc_id
                attrType `shouldBe` (TypedValue NCInt ())
        context "nc_inq_attlen" $ do
            it "returns correct attribute length" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "scalar_int"
                attrLen                <- checkNC =<< nc_inq_attlen nc_id (Just var) "many_ints"
                _                      <- checkNC =<< nc_close nc_id
                attrLen `shouldBe` 3
            it "returns correct global attribute length" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                attrLen                <- checkNC =<< nc_inq_attlen nc_id Nothing "version"
                _                      <- checkNC =<< nc_close nc_id
                attrLen `shouldBe` 1
        context "nc_get_att" $ do
            it "correctly reads attribute - 1" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "scalar_int"
                (TypedValue t v)       <- checkNC =<< nc_get_att nc_id (Just var) "many_ints"
                _                      <- checkNC =<< nc_close nc_id
                case t of
                    NCInt64 -> v `shouldBe` VS.fromList [3, 7, 21]
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads attribute - 2" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "string_vector"
                (TypedValue t v)       <- checkNC =<< nc_get_att nc_id (Just var) "long_name"
                _                      <- checkNC =<< nc_close nc_id
                case t of
                    NCString -> do
                        let ncStringPtr = v VS.! 0
                        nc_str  <- fromNCString ncStringPtr
                        _       <- nc_free_string ncStringPtr
                        nc_str `shouldBe` "a vector of strings"
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a global attribute" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (TypedValue t v)       <- checkNC =<< nc_get_att nc_id Nothing "version"
                _                      <- checkNC =<< nc_close nc_id
                case t of
                    NCInt -> v `shouldBe` VS.fromList [2021]
                    _ -> expectationFailure "Unexpected data type"
        context "nc_put_att" $ do
            it "correctly sets attribute - 1" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr1_put.nc") NCNetCDF4 NCClobber
                var_id                 <- checkNC =<< nc_def_scalar_var nc_id "variable" NCInt
                _                      <- checkNC =<< nc_put_att nc_id (Just var_id) "attribute" NCInt64 [1,2,3]
                (TypedValue t v)       <- checkNC =<< nc_get_att nc_id (Just var_id) "attribute"
                _                      <- checkNC =<< nc_close nc_id
                case t of
                    NCInt64 -> do
                        v `shouldBe` VS.fromList [1,2,3]
                    _ -> expectationFailure "Unexpected data type"
            it "correctly sets attribute - 2" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr2_put.nc") NCNetCDF4 NCClobber
                var_id                 <- checkNC =<< nc_def_scalar_var nc_id "variable" NCInt
                _                      <- checkNC =<< nc_put_att nc_id (Just var_id) "attribute" NCInt64 (VS.fromList [4,5,6])
                (TypedValue t v)       <- checkNC =<< nc_get_att nc_id (Just var_id) "attribute"
                _                      <- checkNC =<< nc_close nc_id
                case t of
                    NCInt64 -> do
                        v `shouldBe` VS.fromList [4,5,6]
                    _ -> expectationFailure "Unexpected data type"
            it "correctly sets attribute - 3" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr3_put.nc") NCNetCDF4 NCClobber
                _                      <- checkNC =<< nc_put_att nc_id Nothing "attribute" NCFloat [1.2,3.4]
                (TypedValue t v)       <- checkNC =<< nc_get_att nc_id Nothing "attribute"
                _                      <- checkNC =<< nc_close nc_id
                case t of
                    NCFloat -> do
                        v `shouldBe` VS.fromList [1.2,3.4]
                    _ -> expectationFailure "Unexpected data type"
        context "nc_rename_att" $ do
            it "correctly renames attribute" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr1_rename.nc") NCNetCDF4 NCClobber
                _                      <- checkNC =<< nc_put_att nc_id Nothing "attribute" NCFloat [1.2,3.4]
                _                      <- checkNC =<< nc_rename_att nc_id Nothing "attribute" "new name"
                (TypedValue t v)       <- checkNC =<< nc_get_att nc_id Nothing "new name"
                _                      <- checkNC =<< nc_close nc_id
                case t of
                    NCFloat -> do
                        v `shouldBe` VS.fromList [1.2,3.4]
                    _ -> expectationFailure "Unexpected data type"
        context "nc_del_att" $ do
            it "correctly deletes attribute" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr1_delete.nc") NCNetCDF4 NCClobber
                forM_ [1,2,3] $ \i -> do
                    let attr_name = "attribute_" ++ show i
                    checkNC =<< nc_put_att nc_id Nothing attr_name NCInt [i]
                numAttr1               <- checkNC =<< nc_inq_natts nc_id
                numAttr1 `shouldBe` 3
                _                      <- checkNC =<< nc_del_att nc_id Nothing "attribute_2"
                numAttr2               <- checkNC =<< nc_inq_natts nc_id
                _                      <- checkNC =<< nc_close nc_id
                numAttr2 `shouldBe` 2
