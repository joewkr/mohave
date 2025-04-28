{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Format.NetCDF.LowLevel.AttributeSpec(spec) where

import           Control.Monad (forM_, void)
import qualified Data.Vector.Storable as VS
import           Foreign.Storable (sizeOf)
import           Test.Hspec
import           System.FilePath ((</>))

import           Data.Format.NetCDF.LowLevel
import           Data.Format.NetCDF.LowLevel.Attribute
import           Data.Format.NetCDF.LowLevel.File
import           Data.Format.NetCDF.LowLevel.String
import           Data.Format.NetCDF.LowLevel.User.Type
import           Data.Format.NetCDF.LowLevel.Variable

import           Data.Format.NetCDF.User.Types
import           Testing.Common

spec :: Spec
spec = do
    describe "Attributes" $ do
        context "nc_inq_att" $ do
            it "returns correct attribute info" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "scalar_int"
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id (Just var) "many_ints"
                _                      <- checkNC =<< nc_close nc_id
                case t of
                    SNCInt64 -> do
                        (ncAttributeName    at) `shouldBe` "many_ints"
                        (ncAttributeNValues at) `shouldBe` 3
                    _ -> expectationFailure "Unexpected data type"
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
                (SomeNCType NCType{ncTypeTag=t}) <- checkNC =<< nc_inq_atttype nc_id (Just var) "test_attr"
                _                      <- checkNC =<< nc_close nc_id
                case t of
                    SNCDouble -> return ()
                    _ -> expectationFailure "Unexpected data type"
            it "returns correct global attribute type" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCType NCType{ncTypeTag=t}) <- checkNC =<< nc_inq_atttype nc_id Nothing "version"
                _                      <- checkNC =<< nc_close nc_id
                case t of
                    SNCInt -> return ()
                    _ -> expectationFailure "Unexpected data type"
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
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id (Just var) "many_ints"
                case t of
                    SNCInt64 -> do
                        v  <- checkNC =<< nc_get_att nc_id at
                        v `shouldBe` VS.fromList [3, 7, 21]
                    _ -> expectationFailure "Unexpected data type"
                void $                    checkNC =<< nc_close nc_id
            it "correctly reads attribute - 2" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "string_vector"
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id (Just var) "long_name"
                case t of
                    SNCString -> do
                        v       <- checkNC =<< nc_get_att nc_id at
                        let ncStringPtr = v VS.! 0
                        nc_str  <- fromNCString ncStringPtr
                        _       <- nc_free_string ncStringPtr
                        nc_str `shouldBe` "a vector of strings"
                    _ -> expectationFailure "Unexpected data type"
                void $                    checkNC =<< nc_close nc_id
            it "correctly reads compound attribute" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "scalar_int"
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id (Just var) "compound_attribute"
                case t of
                    SCompoundAttr -> do
                        v  <- checkNC =<< nc_get_att nc_id at
                        v `shouldBe` VS.fromList [CompoundAttr 1440 3]
                    _ -> expectationFailure $ "Unexpected data type:\t" ++ show t
                void $                    checkNC =<< nc_close nc_id
            it "correctly reads opaque attribute" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "scalar_opaque"
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id (Just var) "attr"
                case t of
                    SOpaqueMB -> do
                        v  <- checkNC =<< nc_get_att nc_id at
                        v `shouldBe` VS.fromList [OpaqueMB $ Just False, OpaqueMB Nothing]
                    _ -> expectationFailure $ "Unexpected data type:\t" ++ show t
                void $                    checkNC =<< nc_close nc_id
            it "correctly reads enum attribute" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "scalar_enum"
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id (Just var) "attr"
                case t of
                    (SNCEnum SNCShort) -> do
                        v  <- checkNC =<< nc_get_att nc_id at
                        v `shouldBe` VS.fromList [999,0,1]
                    _ -> expectationFailure $ "Unexpected data type:\t" ++ show t
                void $                    checkNC =<< nc_close nc_id
            it "correctly reads a global attribute" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id Nothing "version"
                case t of
                    SNCInt -> do
                        v <- checkNC =<< nc_get_att nc_id at
                        v `shouldBe` VS.fromList [2021]
                    _ -> expectationFailure "Unexpected data type"
                void $                    checkNC =<< nc_close nc_id
        context "nc_put_att" $ do
            it "correctly sets attribute - 1" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr1_put.nc") NCNetCDF4 NCClobber
                var_id                 <- checkNC =<< nc_def_scalar_var nc_id "variable" NCInt
                at                     <- checkNC =<< nc_put_att nc_id (Just var_id) "attribute" NCInt64 [1,2,3]
                v                      <- checkNC =<< nc_get_att nc_id at
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` VS.fromList [1,2,3]
            it "correctly sets attribute - 2" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr2_put.nc") NCNetCDF4 NCClobber
                var_id                 <- checkNC =<< nc_def_scalar_var nc_id "variable" NCInt
                at                     <- checkNC =<< nc_put_att nc_id (Just var_id) "attribute" NCInt64 (VS.fromList [4,5,6])
                v                      <- checkNC =<< nc_get_att nc_id at
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` VS.fromList [4,5,6]
            it "correctly sets attribute - 3" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr3_put.nc") NCNetCDF4 NCClobber
                at                     <- checkNC =<< nc_put_att nc_id Nothing "attribute" NCFloat [1.2,3.4]
                v                      <- checkNC =<< nc_get_att nc_id at
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` VS.fromList [1.2,3.4]
            it "correctly sets compound attribute - 1" $ do
                let nc_data = [CompoundAttr 57 9, CompoundAttr 23 32]
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr4_put.nc") NCNetCDF4 NCClobber
                type_id0               <- checkNC =<< nc_def_compound nc_id (fromIntegral $ sizeOf (undefined :: CompoundAttr)) "compound_attr_type"
                type_id1               <- checkNC =<< nc_insert_compound nc_id type_id0 "aa" (ST0 STBot) NCUInt
                type_id2               <- checkNC =<< nc_insert_compound nc_id type_id1 "bb" (ST1 (ST1 STBot)) NCUByte

                at                     <- checkNC =<< nc_put_att nc_id Nothing "attribute" type_id2 nc_data
                v                      <- checkNC =<< nc_get_att nc_id at
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` VS.fromList nc_data
            it "correctly sets opaque attribute - 1" $ do
                let nc_data = [BinaryBlob64 "ZZXXYY", BinaryBlob64 "54b7cc7fb8cd0db4df2c70f9d1a4d0d598b10abc"]
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr5_put.nc") NCNetCDF4 NCClobber
                type_id                <- checkNC =<< nc_def_opaque nc_id [snat3|65|] "blob64"

                at                     <- checkNC =<< nc_put_att nc_id Nothing "attribute" type_id nc_data
                v                      <- checkNC =<< nc_get_att nc_id at
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` VS.fromList nc_data
            it "correctly sets enum attribute - 1" $ do
                let nc_data = [1,2,0]
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr6_put.nc") NCNetCDF4 NCClobber
                type_id                <- checkNC =<< nc_def_enum nc_id NCShort "precip_type"
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "None" 0
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "Rain" 1
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "Snow" 2
                at                     <- checkNC =<< nc_put_att nc_id Nothing "attribute" type_id nc_data
                v                      <- checkNC =<< nc_get_att nc_id at
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` VS.fromList nc_data
        context "nc_rename_att" $ do
            it "correctly renames attribute" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr1_rename.nc") NCNetCDF4 NCClobber
                _                      <- checkNC =<< nc_put_att nc_id Nothing "attribute" NCFloat [1.2,3.4]
                _                      <- checkNC =<< nc_rename_att nc_id Nothing "attribute" "new name"
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id Nothing "new name"
                case t of
                    SNCFloat -> do
                        v <- checkNC =<< nc_get_att nc_id at
                        v `shouldBe` VS.fromList [1.2,3.4]
                    _ -> expectationFailure "Unexpected data type"
                void $                    checkNC =<< nc_close nc_id
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
        context "nc_get_scalar_att" $ do
            it "correctly reads scalar attribute - 1" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "string_vector"
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id (Just var) "test_attr"
                case t of
                    SNCDouble -> do
                        v <- checkNC =<< nc_get_scalar_att nc_id at
                        v `shouldBe` 1.5
                    _ -> expectationFailure "Unexpected data type"
                void $                     checkNC =<< nc_close nc_id
            it "correctly reads scalar attribute - 2" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "string_vector"
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id  (Just var) "long_name"
                case t of
                    SNCString -> do
                        v <- checkNC =<< nc_get_scalar_att nc_id at
                        nc_str  <- fromNCString v
                        _       <- nc_free_string v
                        nc_str `shouldBe` "a vector of strings"
                    _ -> expectationFailure "Unexpected data type"
                void $                    checkNC =<< nc_close nc_id
            it "correctly reads scalar attribute - 3" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "scalar_int"
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id (Just var) "many_ints"
                case t of
                    SNCInt64 -> do
                        v <- checkNC =<< nc_get_scalar_att nc_id at
                        v `shouldBe` 3
                    _ -> expectationFailure "Unexpected data type"
                void $                    checkNC =<< nc_close nc_id
            it "correctly reads a global scalar attribute" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id Nothing "version"
                case t of
                    SNCInt -> do
                        v <- checkNC =<< nc_get_scalar_att nc_id at
                        v `shouldBe` 2021
                    _ -> expectationFailure "Unexpected data type"
                void $                    checkNC =<< nc_close nc_id
            it "correctly reads a scalar compound attribute" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "scalar_int"
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id (Just var) "compound_attribute"
                case t of
                    SCompoundAttr -> do
                        v  <- checkNC =<< nc_get_scalar_att nc_id at
                        v `shouldBe` CompoundAttr 1440 3
                    _ -> expectationFailure $ "Unexpected data type:\t" ++ show t
                void $                    checkNC =<< nc_close nc_id
            it "correctly reads a scalar opaque attribute" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "scalar_opaque"
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id (Just var) "scalar attr"
                case t of
                    SOpaqueMB -> do
                        v  <- checkNC =<< nc_get_scalar_att nc_id at
                        v `shouldBe` (OpaqueMB $ Just True)
                    _ -> expectationFailure $ "Unexpected data type:\t" ++ show t
                void $                    checkNC =<< nc_close nc_id
            it "correctly reads a scalar enum attribute" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "scalar_enum"
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id (Just var) "scalar attr"
                case t of
                    (SNCEnum SNCShort) -> do
                        v  <- checkNC =<< nc_get_scalar_att nc_id at
                        v `shouldBe` 1
                    _ -> expectationFailure $ "Unexpected data type:\t" ++ show t
                void $                    checkNC =<< nc_close nc_id
        context "nc_put_scalar_att" $ do
            it "correctly sets scalar attribute - 1" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr3_put.nc") NCNetCDF4 NCClobber
                var_id                 <- checkNC =<< nc_def_scalar_var nc_id "variable" NCInt
                at                     <- checkNC =<< nc_put_scalar_att nc_id (Just var_id) "attribute" NCInt64 11
                v                      <- checkNC =<< nc_get_scalar_att nc_id at
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` 11
            it "correctly sets scalar attribute - 2" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr1_put_s.nc") NCNetCDF4 NCClobber
                at                     <- checkNC =<< nc_put_scalar_att nc_id Nothing "attribute" NCFloat 21.1
                v                      <- checkNC =<< nc_get_scalar_att nc_id at
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` 21.1
            it "correctly sets scalar compound attribute" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr1_put_sc.nc") NCNetCDF4 NCClobber
                type_id0               <- checkNC =<< nc_def_compound nc_id (fromIntegral $ sizeOf (undefined :: CompoundAttr)) "compound_attr_type"
                type_id1               <- checkNC =<< nc_insert_compound nc_id type_id0 "ii" (ST0 STBot) NCUInt
                type_id2               <- checkNC =<< nc_insert_compound nc_id type_id1 "jj" (ST1 (ST1 STBot)) NCUByte

                at                     <- checkNC =<< nc_put_scalar_att nc_id Nothing "attribute" type_id2 (CompoundAttr 7 99)
                v                      <- checkNC =<< nc_get_scalar_att nc_id at
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` CompoundAttr 7 99
            it "correctly sets scalar opaque attribute" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr1_put_so.nc") NCNetCDF4 NCClobber
                type_id                <- checkNC =<< nc_def_opaque nc_id [snat3|65|] "blob64"

                at                     <- checkNC =<< nc_put_scalar_att nc_id Nothing "attribute" type_id (BinaryBlob64 "ZZXXYY")
                v                      <- checkNC =<< nc_get_scalar_att nc_id at
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` BinaryBlob64 "ZZXXYY"
            it "correctly sets scalar enum attribute" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "attr1_put_se.nc") NCNetCDF4 NCClobber
                type_id                <- checkNC =<< nc_def_enum nc_id NCShort "precip_type"
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "None" 0
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "Rain" 1
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "Snow" 2
                at                     <- checkNC =<< nc_put_scalar_att nc_id Nothing "attribute" type_id 1
                v                      <- checkNC =<< nc_get_scalar_att nc_id at
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` 1
