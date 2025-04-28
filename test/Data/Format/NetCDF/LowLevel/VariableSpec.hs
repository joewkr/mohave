{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Format.NetCDF.LowLevel.VariableSpec(spec) where

import           Control.Monad (void)
import           Data.Int (Int64)
import qualified Data.Vector.Storable as VS
import           Foreign.Storable (sizeOf)
import           GHC.TypeNats
import           System.FilePath ((</>))
import           Test.Hspec

import           Data.Format.NetCDF.LowLevel
import           Data.Format.NetCDF.LowLevel.File
import           Data.Format.NetCDF.LowLevel.Dimension
import           Data.Format.NetCDF.LowLevel.Variable
import           Data.Format.NetCDF.LowLevel.User.Type
import           Data.Format.NetCDF.LowLevel.Util (ncVarNDimsProxy)

import           Data.Format.NetCDF.User.Types
import           Testing.Common

spec :: Spec
spec = do
    describe "Variables" $ do
        context "nc_def_dim" $ do
            it "correctly defines a new variable" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var1.nc") NCNetCDF4 NCClobber
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 10)
                _                      <- checkNC =<< nc_def_var nc_id "variable" NCInt64 (D dim_id :| dim_id)
                _                      <- checkNC =<< nc_close nc_id
                return ()
            it "correctly defines a new scalar variable" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var2.nc") NCNetCDF4 NCClobber
                _                      <- checkNC =<< nc_def_scalar_var nc_id "variable" NCInt64
                _                      <- checkNC =<< nc_close nc_id
                return ()
        context "nc_def_var_fill" $ do
            it "correctly sets a fill value" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var1_fill.nc") NCNetCDF4 NCClobber
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 10)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" NCInt64 (D dim_id)
                _                      <- checkNC =<< nc_def_var_fill nc_id var_id (Just 7)
                _                      <- checkNC =<< nc_close nc_id
                return ()
        context "nc_def_var_deflate" $ do
            it "correctly sets compression" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var1_deflate.nc") NCNetCDF4 NCClobber
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 10)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" NCInt64 (D dim_id)
                _                      <- checkNC =<< nc_def_var_deflate nc_id var_id True (Just 9)
                _                      <- checkNC =<< nc_close nc_id
                return ()
        context "nc_def_var_fletcher32" $ do
            it "correctly sets check sum" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var1_fletcher32.nc") NCNetCDF4 NCClobber
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 10)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" NCInt64 (D dim_id)
                _                      <- checkNC =<< nc_def_var_fletcher32 nc_id var_id True
                _                      <- checkNC =<< nc_close nc_id
                return ()
        context "nc_def_var_chunking" $ do
            it "correctly sets chunked variable" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var1_chunking_yes.nc") NCNetCDF4 NCClobber
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 10)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" NCInt64 (D dim_id :| dim_id)
                _                      <- checkNC =<< nc_def_var_chunking nc_id var_id (Just $ D 5 :| 5)
                _                      <- checkNC =<< nc_close nc_id
                return ()
            it "correctly sets contiguous variable" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var1_chunking_no.nc") NCNetCDF4 NCClobber
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 10)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" NCInt64 (D dim_id :| dim_id)
                _                      <- checkNC =<< nc_def_var_chunking nc_id var_id Nothing
                _                      <- checkNC =<< nc_close nc_id
                return ()
        context "nc_def_var_endian" $ do
            it "correctly sets endianness - big" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var1_be.nc") NCNetCDF4 NCClobber
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 10)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" NCInt64 (D dim_id)
                _                      <- checkNC =<< nc_def_var_endian nc_id var_id NCEndianBig
                _                      <- checkNC =<< nc_close nc_id
                return ()
            it "correctly sets endianness - little" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var1_le.nc") NCNetCDF4 NCClobber
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 10)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" NCInt64 (D dim_id)
                _                      <- checkNC =<< nc_def_var_endian nc_id var_id NCEndianLittle
                _                      <- checkNC =<< nc_close nc_id
                return ()
            it "correctly sets endianness - native" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var1_ne.nc") NCNetCDF4 NCClobber
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 10)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" NCInt64 (D dim_id)
                _                      <- checkNC =<< nc_def_var_endian nc_id var_id NCEndianNative
                _                      <- checkNC =<< nc_close nc_id
                return ()
        context "nc_rename_var" $ do
            it "correctly renames variable" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var1_new_name.nc") NCNetCDF4 NCClobber
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 10)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" NCInt64 (D dim_id)
                _                      <- checkNC =<< nc_rename_var nc_id var_id "new_name"
                varName                <- checkNC =<< nc_inq_varname nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                varName `shouldBe` "new_name"
        context "nc_inq_varid" $ do
            it "correctly selects a variable - vector" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                _                      <- checkNC =<< nc_inq_varid nc_id "vector_var"
                _                      <- checkNC =<< nc_close nc_id
                return ()
            it "correctly selects a variable - scalar" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                _                      <- checkNC =<< nc_inq_varid nc_id "scalar_var"
                _                      <- checkNC =<< nc_close nc_id
                return ()
        context "nc_inq_varndims" $ do
            it "correctly returns variable rank - array" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ var) <-
                                          checkNC =<< nc_inq_varid nc_id "vector_var"
                numDims                <- checkNC =<< nc_inq_varndims nc_id var
                _                      <- checkNC =<< nc_close nc_id
                numDims `shouldBe` (fromIntegral . natVal $ ncVarNDimsProxy var)
            it "correctly returns variable rank - scalar" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ var) <-
                                          checkNC =<< nc_inq_varid nc_id "scalar_var"
                numDims                <- checkNC =<< nc_inq_varndims nc_id var
                _                      <- checkNC =<< nc_close nc_id
                numDims `shouldBe` (fromIntegral . natVal $ ncVarNDimsProxy var)
        context "nc_inq_vartype" $ do
            it "correctly returns variable type" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable t  var) <-
                                          checkNC =<< nc_inq_varid nc_id "vector_var"
                (SomeNCType NCType{ncTypeTag=t2}) <- checkNC =<< nc_inq_vartype nc_id var
                _                      <- checkNC =<< nc_close nc_id
                case (t,t2) of
                    (SNCInt,SNCInt) -> return ()
                    _ -> expectationFailure $ "Unexpected variable data type: " ++ show (t,t2)
        context "nc_inq_varname" $ do
            it "correctly returns variable name" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ var) <-
                                          checkNC =<< nc_inq_varid nc_id "scalar_var"
                varName                <- checkNC =<< nc_inq_varname nc_id var
                _                      <- checkNC =<< nc_close nc_id
                varName `shouldBe` "scalar_var"
        context "nc_inq_vardimid" $ do
            it "correctly returns variable dimensions" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ var) <-
                                          checkNC =<< nc_inq_varid nc_id "var_2d"
                dim1_id                <- checkNC =<< nc_inq_dimid nc_id "dim1"
                dim2_id                <- checkNC =<< nc_inq_dimid nc_id "dim2"
                varDims                <- checkNC =<< nc_inq_vardimid nc_id var
                _                      <- checkNC =<< nc_close nc_id
                ((fromStaticVector <$> varDims) == Just [dim1_id, dim2_id]) `shouldBe` True
                case ncVarNDimsProxy var of
                    Var2D -> (varDims == Just (D dim1_id :| dim2_id)) `shouldBe` True
                    _     -> expectationFailure $ "Unexpected rank: " ++ (show . natVal $ ncVarNDimsProxy var)
            it "correctly handles scalar variables" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "scalar_var"
                varDims                <- checkNC =<< nc_inq_vardimid nc_id var
                _                      <- checkNC =<< nc_close nc_id
                varDims == Nothing `shouldBe` True
        context "nc_inq_varnatts" $ do
            it "correctly returns number of associated attributes - 1" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "scalar_var"
                numAtts                <- checkNC =<< nc_inq_varnatts nc_id var
                _                      <- checkNC =<< nc_close nc_id
                numAtts `shouldBe` 2
            it "correctly returns number of associated attributes - 2" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "dummy_var"
                numAtts                <- checkNC =<< nc_inq_varnatts nc_id var
                _                      <- checkNC =<< nc_close nc_id
                numAtts `shouldBe` 0
        context "nc_inq_var_deflate" $ do
            it "correctly returns deflate parameters - 1" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "vector_var"
                deflateParams          <- checkNC =<< nc_inq_var_deflate nc_id var
                _                      <- checkNC =<< nc_close nc_id
                deflateParams `shouldBe` (NCDeflateParams False $ Just 9)
            it "correctly returns deflate parameters - 2" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "scalar_var"
                deflateParams          <- checkNC =<< nc_inq_var_deflate nc_id var
                _                      <- checkNC =<< nc_close nc_id
                deflateParams `shouldBe` (NCDeflateParams False Nothing)
            it "correctly returns deflate parameters - 3" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "var_2d"
                deflateParams          <- checkNC =<< nc_inq_var_deflate nc_id var
                _                      <- checkNC =<< nc_close nc_id
                deflateParams `shouldBe` (NCDeflateParams True Nothing)
            it "correctly returns deflate parameters - 4" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "var_2d_compressed4"
                deflateParams          <- checkNC =<< nc_inq_var_deflate nc_id var
                _                      <- checkNC =<< nc_close nc_id
                deflateParams `shouldBe` (NCDeflateParams True $ Just 4)
            it "correctly returns deflate parameters - 5" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "var_2d_compressed0"
                deflateParams          <- checkNC =<< nc_inq_var_deflate nc_id var
                _                      <- checkNC =<< nc_close nc_id
                deflateParams `shouldBe` (NCDeflateParams True Nothing)
        context "nc_inq_var_fletcher32" $ do
            it "correctly reports whether checksum is used - 1" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "vector_var"
                fletcher32             <- checkNC =<< nc_inq_var_fletcher32 nc_id var
                _                      <- checkNC =<< nc_close nc_id
                fletcher32 `shouldBe` True
            it "correctly reports whether checksum is used - 2" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "scalar_var"
                fletcher32             <- checkNC =<< nc_inq_var_fletcher32 nc_id var
                _                      <- checkNC =<< nc_close nc_id
                fletcher32 `shouldBe` False
        context "nc_inq_var_chunking" $ do
            it "correctly reports chunking parameters" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ var) <-
                                          checkNC =<< nc_inq_varid nc_id "var_2d"
                chunks                 <- checkNC =<< nc_inq_var_chunking nc_id var
                _                      <- checkNC =<< nc_close nc_id
                (fromStaticVector <$> chunks) `shouldBe` (Just [2, 3])
                case ncVarNDimsProxy var of
                    Var2D -> chunks `shouldBe` Just (D 2 :| 3)
                    _     -> expectationFailure $ "Unexpected rank: " ++ (show . natVal $ ncVarNDimsProxy var)
        context "nc_inq_var_fill" $ do
            it "correctly reports filling value" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_var"
                case t of
                    SNCInt -> do
                        fillSpec  <- checkNC =<< nc_inq_var_fill nc_id var
                        _         <- checkNC =<< nc_close nc_id
                        fillSpec `shouldBe` (NCFill, 1)
                    _ -> do
                        _         <- checkNC =<< nc_close nc_id
                        expectationFailure "Unexpected NC data type"
        context "nc_inq_var_endian" $ do
            it "correctly reports endianness of a variable - 1" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "dummy_var"
                endianness             <- checkNC =<< nc_inq_var_endian nc_id var
                _                      <- checkNC =<< nc_close nc_id
                endianness `shouldBe` NCEndianLittle
            it "correctly reports endianness of a variable - 2" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ var) <- checkNC =<< nc_inq_varid nc_id "scalar_var"
                endianness             <- checkNC =<< nc_inq_var_endian nc_id var
                _                      <- checkNC =<< nc_close nc_id
                endianness `shouldBe` NCEndianBig
        context "nc_inq_unlimdims" $ do
            it "correctly reports number of unlimited dimensions" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test2.nc" NCNoWrite
                numUnlimDims           <- checkNC =<< nc_inq_num_unlimdims nc_id
                _                      <- checkNC =<< nc_close nc_id
                numUnlimDims `shouldBe` 3
            it "correctly reports unlimited dimensions" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test2.nc" NCNoWrite
                dim1_id                <- checkNC =<< nc_inq_dimid nc_id "dim1"
                dim2_id                <- checkNC =<< nc_inq_dimid nc_id "dim2"
                dim3_id                <- checkNC =<< nc_inq_dimid nc_id "dim3"
                unlimDims              <- checkNC =<< nc_inq_unlimdims nc_id
                _                      <- checkNC =<< nc_close nc_id
                unlimDims == [dim1_id, dim2_id, dim3_id] `shouldBe` True
        context "nc_get_vara" $ do
            it "correctly reads a vector variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <-
                                          checkNC =<< nc_inq_varid nc_id "vector_int"
                case t of
                    SNCInt -> case ncVarNDimsProxy var of
                        Var1D -> do
                            nc_data <- checkNC =<< nc_get_vara nc_id var (D 0) (D 3)
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` VS.fromList [3, 4, 5]
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a subset of a vector variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <-
                                          checkNC =<< nc_inq_varid nc_id "vector_int"
                case t of
                    SNCInt -> case ncVarNDimsProxy var of
                        Var1D -> do
                            nc_data <- checkNC =<< nc_get_vara nc_id var (D 1) (D 1)
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` VS.fromList [4]
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a compound vector variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_compound"
                case t of
                    SCompound -> case ncVarNDimsProxy var of
                        Var1D -> do
                            nc_data <- checkNC =<< nc_get_vara nc_id var (D 0) (D 2)
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` VS.fromList [Compound 17 0.1 23.45, Compound (-1) 22.44 1.0E+20]
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads an enum vector variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_enum"
                case t of
                    (SNCEnum SNCByte) -> case ncVarNDimsProxy var of
                        Var1D -> do
                            nc_data <- checkNC =<< nc_get_vara nc_id var (D 0) (D 2)
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` VS.fromList [127, 0]
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
        context "nc_get_var1" $ do
            it "correctly reads a single value from vector" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <-
                                          checkNC =<< nc_inq_varid nc_id "vector_int"
                case t of
                    SNCInt -> case ncVarNDimsProxy var of
                        Var1D -> do
                            nc_data <- checkNC =<< nc_get_var1 nc_id var (D 1)
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` 4
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a single value from compound vector variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_compound"
                case t of
                    SCompound -> case ncVarNDimsProxy var of
                        Var1D -> do
                            nc_data <- checkNC =<< nc_get_var1 nc_id var (D 2)
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` (Compound 22 1.0 5.7)
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a single value from an enum vector variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_enum"
                case t of
                    (SNCEnum SNCByte) -> case ncVarNDimsProxy var of
                        Var1D -> do
                            nc_data <- checkNC =<< nc_get_var1 nc_id var (D 2)
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` 1
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
        context "nc_get_var" $ do
            it "correctly reads a scalar variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "scalar_int"
                case t of
                    SNCInt -> do
                        nc_data <- checkNC =<< nc_get_var nc_id var
                        _       <- checkNC =<< nc_close nc_id
                        nc_data `shouldBe` VS.fromList [7]
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a vector variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_int"
                case t of
                    SNCInt -> do
                        nc_data <- checkNC =<< nc_get_var nc_id var
                        _       <- checkNC =<< nc_close nc_id
                        nc_data `shouldBe` VS.fromList [3,4,5]
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a scalar compound variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "scalar_compound"
                case t of
                    SCompound -> do
                        nc_data <- checkNC =<< nc_get_var nc_id var
                        _       <- checkNC =<< nc_close nc_id
                        nc_data `shouldBe` VS.fromList [Compound 18 0.2 34.56]
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a vector compound variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_compound"
                case t of
                    SCompound -> do
                        nc_data <- checkNC =<< nc_get_var nc_id var
                        _       <- checkNC =<< nc_close nc_id
                        nc_data `shouldBe` VS.fromList [Compound 17 0.1 23.45, Compound (-1) 22.44 1.0E+20, Compound 22 1 5.7]
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a scalar enum variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "scalar_enum"
                case t of
                    (SNCEnum SNCByte) -> do
                        nc_data <- checkNC =<< nc_get_var nc_id var
                        _       <- checkNC =<< nc_close nc_id
                        nc_data `shouldBe` VS.fromList [2]
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a vector enum variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_enum"
                case t of
                    (SNCEnum SNCByte) -> do
                        nc_data <- checkNC =<< nc_get_var nc_id var
                        _       <- checkNC =<< nc_close nc_id
                        nc_data `shouldBe` VS.fromList [127, 0, 1]
                    _ -> expectationFailure "Unexpected data type"
        context "nc_get_vars" $ do
            it "correctly reads a vector variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_int"
                case t of
                    SNCInt -> case ncVarNDimsProxy var of
                        Var1D -> do
                            nc_data <- checkNC =<< nc_get_vars nc_id var (D 0) (D 2) (D 2)
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` VS.fromList [3,5]
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a vector compound variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_compound"
                case t of
                    SCompound -> case ncVarNDimsProxy var of
                        Var1D -> do
                            nc_data <- checkNC =<< nc_get_vars nc_id var (D 0) (D 2) (D 2)
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` VS.fromList [Compound 17 0.1 23.45, Compound 22 1 5.7]
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a vector enum variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_enum"
                case t of
                    (SNCEnum SNCByte) -> case ncVarNDimsProxy var of
                        Var1D -> do
                            nc_data <- checkNC =<< nc_get_vars nc_id var (D 0) (D 2) (D 2)
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` VS.fromList [127, 1]
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
        context "nc_put_vara" $ do
            it "correctly writes a vector variable" $ do
                let nc_data = VS.fromList ([4,5,6,7] :: [Int64] )
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var1_write.nc") NCNetCDF4 NCClobber
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 4)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" NCInt64 (D dim_id)
                _                      <- checkNC =<< nc_put_vara nc_id var_id (D 0) (D 4) nc_data
                v                      <- checkNC =<< nc_get_var nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` nc_data
            it "correctly writes a vector compound variable" $ do
                let nc_data = VS.fromList [Compound 1 2 3, Compound 22 77.88 31.95]
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var1_write_c.nc") NCNetCDF4 NCClobber
                type_id0               <- checkNC =<< nc_def_compound nc_id (fromIntegral . sizeOf $ nc_data VS.! 0) "compound_type"
                type_id1               <- checkNC =<< nc_insert_compound nc_id type_id0 "integer_field" (ST0 STBot) NCInt
                type_id2               <- checkNC =<< nc_insert_compound nc_id type_id1 "xx" (ST1 (ST1 STBot)) NCFloat
                type_id3               <- checkNC =<< nc_insert_compound nc_id type_id2 "yy" (ST2 (ST2 STBot)) NCFloat

                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 2)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" type_id3 (D dim_id)
                _                      <- checkNC =<< nc_put_vara nc_id var_id (D 0) (D 2) nc_data
                v                      <- checkNC =<< nc_get_var nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` nc_data
            it "correctly writes a vector compound variable - sparse" $ do
                let
                    fileName = testOutputPath </> "var1_write_cs.nc"
                    nc_data  = VS.fromList [Compound 1 2 3, Compound 22 77.88 31.95]
                    nc_dataS = VS.fromList [CompoundSparseA 3 1, CompoundSparseA 31.95 22]
                nc_id                  <- checkNC =<< nc_create fileName NCNetCDF4 NCClobber
                type_id0               <- checkNC =<< nc_def_compound nc_id (fromIntegral . sizeOf $ nc_data VS.! 0) "compound_type"
                type_id1               <- checkNC =<< nc_insert_compound nc_id type_id0 "yy rev" (ST2 (ST2 STBot)) NCFloat
                type_id2               <- checkNC =<< nc_insert_compound nc_id type_id1 "ii rev" (     ST0 STBot ) NCInt

                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 2)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" type_id2 (D dim_id)
                _                      <- checkNC =<< nc_put_vara nc_id var_id (D 0) (D 2) nc_data
                _                      <- checkNC =<< nc_close nc_id

                nc_id                  <- checkNC =<< nc_open fileName NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "variable"
                case t of
                    SCompoundSparseA -> do
                        v  <- checkNC =<< nc_get_var nc_id var
                        v `shouldBe` nc_dataS
                    _ -> expectationFailure "Unexpected data type"
                void $                    checkNC =<< nc_close nc_id
            it "correctly writes a vector opaque variable" $ do
                let
                    fileName = testOutputPath </> "var1_write_o.nc"
                    nc_data  = VS.fromList [OpaqueMB (Just False), OpaqueMB (Just True), OpaqueMB Nothing]
                nc_id                  <- checkNC =<< nc_create fileName NCNetCDF4 NCClobber
                case toTernarySNat (fromIntegral . sizeOf $ nc_data VS.! 0) of
                    SomeTernarySNat n@(ST1 STBot) -> do
                        type_id                <- checkNC =<< nc_def_opaque nc_id n "opaque_type"

                        dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 3)
                        var_id                 <- checkNC =<< nc_def_var nc_id "variable" type_id (D dim_id)
                        _                      <- checkNC =<< nc_put_vara nc_id var_id (D 0) (D 3) nc_data
                        void $                    checkNC =<< nc_close nc_id
                    _ -> expectationFailure "Unexpected opaque data size"

                nc_id                  <- checkNC =<< nc_open fileName NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "variable"
                case t of
                    SOpaqueMB -> do
                        v  <- checkNC =<< nc_get_var nc_id var
                        v `shouldBe` nc_data
                    _ -> expectationFailure "Unexpected data type"
                void $                    checkNC =<< nc_close nc_id
            it "correctly writes a vector enum variable" $ do
                let
                    fileName = testOutputPath </> "var1_write_e.nc"
                    nc_data  = VS.fromList [8,800,555,35,35]
                nc_id                  <- checkNC =<< nc_create fileName NCNetCDF4 NCClobber
                type_id                <- checkNC =<< nc_def_enum nc_id NCInt64 "test_enum_type"
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "A" 8
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "B" 555
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "C" 800
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "D" 35
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 5)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" type_id (D dim_id)
                _                      <- checkNC =<< nc_put_vara nc_id var_id (D 0) (D 5) nc_data
                void $                    checkNC =<< nc_close nc_id

                nc_id                  <- checkNC =<< nc_open fileName NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "variable"
                case t of
                    (SNCEnum SNCInt64) -> do
                        v  <- checkNC =<< nc_get_var nc_id var
                        v `shouldBe` nc_data
                    _ -> expectationFailure "Unexpected data type"
                void $                    checkNC =<< nc_close nc_id
        context "nc_put_var1" $ do
            it "correctly writes a single value" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var2_write.nc") NCNetCDF4 NCClobber
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 2)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" NCInt64 (D dim_id)
                _                      <- checkNC =<< nc_def_var_fill nc_id var_id (Just 100)
                _                      <- checkNC =<< nc_put_var1 nc_id var_id (D 1) 7
                v                      <- checkNC =<< nc_get_var nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` VS.fromList [100, 7]
            it "correctly writes a single compound value" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var2_write_c.nc") NCNetCDF4 NCClobber
                type_id0               <- checkNC =<< nc_def_compound nc_id (fromIntegral $ sizeOf (undefined :: Compound)) "compound_type"
                type_id1               <- checkNC =<< nc_insert_compound nc_id type_id0 "integer_field" (ST0 STBot) NCInt
                type_id2               <- checkNC =<< nc_insert_compound nc_id type_id1 "xx" (ST1 (ST1 STBot)) NCFloat
                type_id3               <- checkNC =<< nc_insert_compound nc_id type_id2 "yy" (ST2 (ST2 STBot)) NCFloat

                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 2)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" type_id3 (D dim_id)
                _                      <- checkNC =<< nc_def_var_fill nc_id var_id (Just $ Compound 999 777 888)
                _                      <- checkNC =<< nc_put_var1 nc_id var_id (D 1) (Compound 1 2 3)
                v                      <- checkNC =<< nc_get_var nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` VS.fromList [Compound 999 777 888, Compound 1 2 3]
            it "correctly writes a single opaque value" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var2_write_o.nc") NCNetCDF4 NCClobber
                type_id                <- checkNC =<< nc_def_opaque nc_id [snat3|1|] "opaque_type"

                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 2)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" type_id (D dim_id)
                _                      <- checkNC =<< nc_def_var_fill nc_id var_id (Just $ (OpaqueMB Nothing))
                _                      <- checkNC =<< nc_put_var1 nc_id var_id (D 1) (OpaqueMB (Just False))
                v                      <- checkNC =<< nc_get_var nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` VS.fromList [OpaqueMB Nothing, OpaqueMB (Just False)]
            it "correctly writes a single enum variable" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var2_write_e.nc") NCNetCDF4 NCClobber
                type_id                <- checkNC =<< nc_def_enum nc_id NCByte "precip_type"
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "None" 0
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "Rain" 1
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "Snow" 2
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 2)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" type_id (D dim_id)
                _                      <- checkNC =<< nc_def_var_fill nc_id var_id (Just 0)
                _                      <- checkNC =<< nc_put_var1 nc_id var_id (D 1) 2
                v                      <- checkNC =<< nc_get_var nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` VS.fromList [0, 2]
        context "nc_put_var" $ do
            it "correctly writes a vector variable" $ do
                let nc_data = VS.fromList ([4,5,6,7] :: [Int64] )
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var3_write.nc") NCNetCDF4 NCClobber
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 4)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" NCInt64 (D dim_id)
                _                      <- checkNC =<< nc_put_var nc_id var_id nc_data
                v                      <- checkNC =<< nc_get_var nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` nc_data
            it "correctly writes a vector compound variable" $ do
                let nc_data = VS.fromList [Compound (-1) (-2) (-3), Compound (-177) 0 (-13.101)]
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var3_write_c.nc") NCNetCDF4 NCClobber
                type_id0               <- checkNC =<< nc_def_compound nc_id (fromIntegral . sizeOf $ nc_data VS.! 0) "compound_type"
                type_id1               <- checkNC =<< nc_insert_compound nc_id type_id0 "integer_field" (ST0 STBot) NCInt
                type_id2               <- checkNC =<< nc_insert_compound nc_id type_id1 "xx" (ST1 (ST1 STBot)) NCFloat
                type_id3               <- checkNC =<< nc_insert_compound nc_id type_id2 "ff" (ST2 (ST2 STBot)) NCFloat

                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 2)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" type_id3 (D dim_id)
                _                      <- checkNC =<< nc_put_var nc_id var_id nc_data
                v                      <- checkNC =<< nc_get_var nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` nc_data
            it "correctly writes a vector opaque value" $ do
                let nc_data = VS.fromList [BinaryBlob64 "IDDQD:IDKFA:impulse 101", BinaryBlob64 "BADC0FFEE0DDF00D"]
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var3_write_o.nc") NCNetCDF4 NCClobber
                type_id                <- checkNC =<< nc_def_opaque nc_id [snat3|65|] "opaque_type"

                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 2)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" type_id (D dim_id)
                _                      <- checkNC =<< nc_put_var nc_id var_id nc_data
                v                      <- checkNC =<< nc_get_var nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` nc_data
            it "correctly writes a vector enum variable" $ do
                let nc_data = VS.fromList [1,2]
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var3_write_e.nc") NCNetCDF4 NCClobber
                type_id                <- checkNC =<< nc_def_enum nc_id NCByte "precip_type"
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "None" 0
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "Rain" 1
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "Snow" 2
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 2)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" type_id (D dim_id)
                _                      <- checkNC =<< nc_put_var nc_id var_id nc_data
                v                      <- checkNC =<< nc_get_var nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` nc_data
        context "nc_put_vars" $ do
            it "correctly writes a vector variable" $ do
                let nc_data = VS.fromList ([2,3] :: [Int64] )
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var4_write.nc") NCNetCDF4 NCClobber
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 4)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" NCInt64 (D dim_id)
                _                      <- checkNC =<< nc_def_var_fill nc_id var_id (Just 100)
                _                      <- checkNC =<< nc_put_vars nc_id var_id (D 0) (D 2) (D 2) nc_data
                v                      <- checkNC =<< nc_get_var nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` VS.fromList [2,100,3,100]
            it "correctly writes a vector compound variable" $ do
                let nc_data = VS.fromList [Compound 1 2 3, Compound 34 5.6 7.8]
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var4_write_c.nc") NCNetCDF4 NCClobber
                type_id0               <- checkNC =<< nc_def_compound nc_id (fromIntegral . sizeOf $ nc_data VS.! 0) "compound_type"
                type_id1               <- checkNC =<< nc_insert_compound nc_id type_id0 "integer_field" (ST0 STBot) NCInt
                type_id2               <- checkNC =<< nc_insert_compound nc_id type_id1 "xx" (ST1 (ST1 STBot)) NCFloat
                type_id3               <- checkNC =<< nc_insert_compound nc_id type_id2 "ff" (ST2 (ST2 STBot)) NCFloat

                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 4)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" type_id3 (D dim_id)
                _                      <- checkNC =<< nc_def_var_fill nc_id var_id (Just $ Compound 123 456 789)
                _                      <- checkNC =<< nc_put_vars nc_id var_id (D 0) (D 2) (D 2) nc_data
                v                      <- checkNC =<< nc_get_var nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` VS.fromList [Compound 1 2 3, Compound 123 456 789, Compound 34 5.6 7.8, Compound 123 456 789]
            it "correctly writes a vector opaque value" $ do
                let nc_data = VS.fromList [OpaqueMB $ Just True, OpaqueMB $ Just False]
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var4_write_o.nc") NCNetCDF4 NCClobber
                type_id                <- checkNC =<< nc_def_opaque nc_id [snat3|1|] "opaque_type"

                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 4)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" type_id (D dim_id)
                _                      <- checkNC =<< nc_def_var_fill nc_id var_id (Just $ (OpaqueMB Nothing))
                _                      <- checkNC =<< nc_put_vars nc_id var_id (D 0) (D 2) (D 2) nc_data
                v                      <- checkNC =<< nc_get_var nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` VS.fromList [OpaqueMB $ Just True, OpaqueMB Nothing, OpaqueMB $ Just False, OpaqueMB Nothing]
            it "correctly writes a vector enum variable" $ do
                let nc_data = VS.fromList [1,2]
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var4_write_e.nc") NCNetCDF4 NCClobber
                type_id                <- checkNC =<< nc_def_enum nc_id NCByte "precip_type"
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "None" 0
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "Rain" 1
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "Snow" 2
                dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 4)
                var_id                 <- checkNC =<< nc_def_var nc_id "variable" type_id (D dim_id)
                _                      <- checkNC =<< nc_def_var_fill nc_id var_id (Just 0)
                _                      <- checkNC =<< nc_put_vars nc_id var_id (D 0) (D 2) (D 2) nc_data
                v                      <- checkNC =<< nc_get_var nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` VS.fromList [1,0,2,0]
        context "nc_get_scalar" $ do
            it "correctly reads a scalar variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <-
                                          checkNC =<< nc_inq_varid nc_id "scalar_int"
                case t of
                    SNCInt -> case ncVarNDimsProxy var of
                        Var0D -> do
                            nc_data <- checkNC =<< nc_get_scalar nc_id var
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` 7
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a scalar compound variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "scalar_compound"
                case t of
                    SCompound -> case ncVarNDimsProxy var of
                        Var0D -> do
                            nc_data <- checkNC =<< nc_get_scalar nc_id var
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` Compound 18 0.2 34.56
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a scalar opaque variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "scalar_opaque"
                case t of
                    SOpaqueBinaryBlob64 -> case ncVarNDimsProxy var of
                        Var0D -> do
                            nc_data <- checkNC =<< nc_get_scalar nc_id var
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` (BinaryBlob64 "\x01\x23\x45\x67\x89\xAB\xCD\xEF\x01\x23\x45\x67\x89\xAB\xCD\xEF")
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a scalar enum variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "scalar_enum"
                case t of
                    (SNCEnum SNCByte) -> case ncVarNDimsProxy var of
                        Var0D -> do
                            nc_data <- checkNC =<< nc_get_scalar nc_id var
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` 2
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
        context "nc_put_scalar" $ do
            it "correctly writes a scalar variable - 1" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var5_write.nc") NCNetCDF4 NCClobber
                var_id                 <- checkNC =<< nc_def_scalar_var nc_id "variable" NCInt64
                _                      <- checkNC =<< nc_put_scalar nc_id var_id 7
                v                      <- checkNC =<< nc_get_scalar nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` 7
            it "correctly writes a scalar variable - 2" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var6_write.nc") NCClassic NCClobber
                var_id                 <- checkNC =<< nc_def_scalar_var nc_id "variable" NCDouble
                _                      <- checkNC =<< nc_enddef nc_id
                _                      <- checkNC =<< nc_put_scalar nc_id var_id 11.5
                v                      <- checkNC =<< nc_get_scalar nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` 11.5
            it "correctly writes a scalar compound variable" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var6_write_c.nc") NCNetCDF4 NCClobber
                type_id0               <- checkNC =<< nc_def_compound nc_id (fromIntegral $ sizeOf (undefined :: Compound)) "compound_type"
                type_id1               <- checkNC =<< nc_insert_compound nc_id type_id0 "integer_field" (ST0 STBot) NCInt
                type_id2               <- checkNC =<< nc_insert_compound nc_id type_id1 "xx" (ST1 (ST1 STBot)) NCFloat
                type_id3               <- checkNC =<< nc_insert_compound nc_id type_id2 "ff" (ST2 (ST2 STBot)) NCFloat
                var_id                 <- checkNC =<< nc_def_scalar_var nc_id "variable" type_id3
                _                      <- checkNC =<< nc_put_scalar nc_id var_id (Compound 41 22.06 7.62)
                v                      <- checkNC =<< nc_get_scalar nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` (Compound 41 22.06 7.62)
            it "correctly writes a scalar opaque variable" $ do
                let nc_data = VS.fromList [OpaqueMB $ Just True, OpaqueMB $ Just False]
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var6_write_o.nc") NCNetCDF4 NCClobber
                type_id                <- checkNC =<< nc_def_opaque nc_id [snat3|1|] "opaque_type"
                var_id                 <- checkNC =<< nc_def_scalar_var nc_id "variable" type_id
                _                      <- checkNC =<< nc_put_scalar nc_id var_id (OpaqueMB $ Just False)
                v                      <- checkNC =<< nc_get_scalar nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` (OpaqueMB $ Just False)
            it "correctly writes a scalar enum variable" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var6_write_e.nc") NCNetCDF4 NCClobber
                type_id                <- checkNC =<< nc_def_enum nc_id NCByte "precip_type"
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "None" 0
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "Rain" 1
                _                      <- checkNC =<< nc_insert_enum nc_id type_id "Snow" 2
                var_id                 <- checkNC =<< nc_def_scalar_var nc_id "variable" type_id
                _                      <- checkNC =<< nc_put_scalar nc_id var_id 2
                v                      <- checkNC =<< nc_get_scalar nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` 2
