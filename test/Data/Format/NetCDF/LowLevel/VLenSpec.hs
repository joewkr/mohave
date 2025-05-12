{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Format.NetCDF.LowLevel.VLenSpec(spec) where

import           Control.Monad (void, forM_)
import           Data.Int (Int8,Int16)
import qualified Data.Vector.Storable as VS
import           System.FilePath ((</>))
import           Test.Hspec

import           Data.Format.NetCDF.LowLevel
import           Data.Format.NetCDF.LowLevel.Attribute
import           Data.Format.NetCDF.LowLevel.File
import           Data.Format.NetCDF.LowLevel.Dimension
import           Data.Format.NetCDF.LowLevel.Variable
import           Data.Format.NetCDF.LowLevel.User.Type
import           Data.Format.NetCDF.LowLevel.Util (ncVarNDimsProxy)
import           Data.Format.NetCDF.LowLevel.VLen

import           Data.Format.NetCDF.User.Types
import           Testing.Common

spec :: Spec
spec = do
    describe "vlen-specific functions" $ do
        context "variables" $ do
            it "nc_get_vara_vlen" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_vlen"
                case t of
                    (SNCVLen SNCDouble) -> case ncVarNDimsProxy var of
                        Var1D -> do
                            nc_vlen_data <- checkNC =<< nc_get_vara_vlen nc_id var (D 0) (D 2)
                            _       <- checkNC =<< nc_close nc_id
                            let expected = zip [0,1..] [VS.fromList [1, 2], VS.fromList [9, 100, (-777)]]
                            forM_ expected $ \(idx,vec) -> do
                                inspectVLenArray nc_vlen_data idx (flip shouldBe $ vec)
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "nc_get_var1_vlen" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_vlen"
                case t of
                    (SNCVLen SNCDouble) -> case ncVarNDimsProxy var of
                        Var1D -> do
                            nc_data <- checkNC =<< nc_get_var1_vlen nc_id var (D 2)
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` VS.fromList [22.33, 77.98, 123.789, 0.554]
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "nc_get_var_vlen" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_vlen_compound"
                case t of
                    (SNCVLen SCompound) -> do
                        nc_vlen_data <- checkNC =<< nc_get_var_vlen nc_id var
                        _            <- checkNC =<< nc_close nc_id
                        let
                            expected_data = [
                                VS.fromList [Compound 7 57.98 12.34, Compound 1 2 11.75]
                              , VS.fromList [Compound 19 0.2 34.56]
                              , VS.fromList [Compound 1 (-12.22) (-13.54), Compound 2 65.56 123.321, Compound 0 0.1 0.2]]
                            expected = zip [0,1..] expected_data
                        forM_ expected $ \(idx,vec) -> do
                            inspectVLenArray nc_vlen_data idx (flip shouldBe $ vec)
                    _ -> expectationFailure "Unexpected data type"
            it "nc_get_vars_vlen" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_vlen_compound"
                case t of
                    (SNCVLen SCompound) -> case ncVarNDimsProxy var of
                        Var1D -> do
                            nc_vlen_data <- checkNC =<< nc_get_vars_vlen nc_id var (D 0) (D 2) (D 2)
                            _            <- checkNC =<< nc_close nc_id
                            let
                                expected_data = [
                                    VS.fromList [Compound 7 57.98 12.34, Compound 1 2 11.75]
                                  , VS.fromList [Compound 1 (-12.22) (-13.54), Compound 2 65.56 123.321, Compound 0 0.1 0.2]]
                                expected = zip [0,1..] expected_data
                            forM_ expected $ \(idx,vec) -> do
                                inspectVLenArray nc_vlen_data idx (flip shouldBe $ vec)
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "nc_get_vlen" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "scalar_vlen"
                case t of
                    (SNCVLen SNCDouble) -> case ncVarNDimsProxy var of
                        Var0D -> do
                            nc_data <- checkNC =<< nc_get_vlen nc_id var
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` VS.fromList [1, 2, 3, 4, 6]
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "nc_put_vlen" $ do
                let nc_data = VS.fromList [1,2,3,4,5]
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "vlen1_write.nc") NCNetCDF4 NCClobber
                type_id                <- checkNC =<< nc_def_vlen nc_id NCByte "vlen_type"
                var_id                 <- checkNC =<< nc_def_scalar_var nc_id "variable" type_id
                _                      <- checkNC =<< nc_put_vlen nc_id var_id nc_data
                v                      <- checkNC =<< nc_get_vlen nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` nc_data
            it "nc_put_var1_vlen" $ do
                let nc_data = VS.fromList [1,2,3,4,5]
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "vlen2_write.nc") NCNetCDF4 NCClobber
                type_id                <- checkNC =<< nc_def_vlen nc_id NCByte "vlen_type"
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 2)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" type_id (D dim_id)
                withVLen VS.empty $ \vlen -> do
                    void $ checkNC =<< nc_def_var_fill nc_id var_id (Just vlen)
                _                      <- checkNC =<< nc_put_var1_vlen nc_id var_id (D 1) nc_data
                nc_vlen_data           <- checkNC =<< nc_get_var_vlen nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                forM_ (zip [0,1] [VS.empty, nc_data]) $ \(idx,vec) -> do
                    inspectVLenArray nc_vlen_data idx (flip shouldBe $ vec)
        context "attributes" $ do
            it "nc_get_vlen_att" $ do
                let nc_data = [VS.fromList [Compound 7 2.54 (-7.99), Compound 1 2.2 3.3], VS.fromList [Compound 5 6.6 7.7]]
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "vector_vlen_compound"
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id (Just var) "attr"
                case t of
                    (SNCVLen SCompound) -> do
                        v  <- checkNC =<< nc_get_vlen_att nc_id at
                        _  <- checkNC =<< nc_close nc_id
                        v `shouldBe` nc_data
                    _ -> expectationFailure $ "Unexpected data type:\t" ++ show t
            it "nc_get_scalar_vlen_att" $ do
                let nc_data = VS.fromList [Compound 7 2.54 (-7.99), Compound 1 2.2 3.3, Compound 5 6.6 7.7]
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "vector_vlen_compound"
                (SomeNCAttribute t at) <- checkNC =<< nc_inq_att nc_id (Just var) "scalar attr"
                case t of
                    (SNCVLen SCompound) -> do
                        v  <- checkNC =<< nc_get_scalar_vlen_att nc_id at
                        v `shouldBe` nc_data
                    _ -> expectationFailure $ "Unexpected data type:\t" ++ show t
                void $                    checkNC =<< nc_close nc_id
            it "nc_put_vlen_att" $ do
                let nc_data = [VS.fromList [1,2], VS.fromList [2,2,0], VS.fromList [1,2,2,1]]
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "vlen1_attr.nc") NCNetCDF4 NCClobber
                type_id0               <- checkNC =<< nc_def_enum nc_id NCShort "precip_type"
                _                      <- checkNC =<< nc_insert_enum nc_id type_id0 "None" 0
                _                      <- checkNC =<< nc_insert_enum nc_id type_id0 "Rain" 1
                _                      <- checkNC =<< nc_insert_enum nc_id type_id0 "Snow" 2
                type_id                <- checkNC =<< nc_def_vlen nc_id type_id0 "vlen_type"
                at                     <- checkNC =<< nc_put_vlen_att nc_id Nothing "attribute" type_id nc_data
                v                      <- checkNC =<< nc_get_vlen_att nc_id at
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` nc_data
            it "nc_put_scalar_vlen_att" $ do
                let nc_data = VS.fromList [1,2,2,0,0,1]
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "vlen2_attr.nc") NCNetCDF4 NCClobber
                type_id0               <- checkNC =<< nc_def_enum nc_id NCShort "precip_type"
                _                      <- checkNC =<< nc_insert_enum nc_id type_id0 "None" 0
                _                      <- checkNC =<< nc_insert_enum nc_id type_id0 "Rain" 1
                _                      <- checkNC =<< nc_insert_enum nc_id type_id0 "Snow" 2
                type_id                <- checkNC =<< nc_def_vlen nc_id type_id0 "vlen_type"
                at                     <- checkNC =<< nc_put_scalar_vlen_att nc_id Nothing "attribute" type_id nc_data
                v                      <- checkNC =<< nc_get_scalar_vlen_att nc_id at
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` nc_data

