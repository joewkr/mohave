{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Format.NetCDF.LowLevel.VariableSpec(spec) where

import           Data.Int (Int64)
import           Data.Proxy
import qualified Data.Vector.Storable as VS
import           GHC.TypeNats
import           System.FilePath ((</>))
import           Test.Hspec

import           Data.Format.NetCDF.LowLevel
import           Data.Format.NetCDF.LowLevel.File
import           Data.Format.NetCDF.LowLevel.Dimension
import           Data.Format.NetCDF.LowLevel.Variable
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
                (SomeNCVariable _ (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "vector_var"
                numDims                <- checkNC =<< nc_inq_varndims nc_id var
                _                      <- checkNC =<< nc_close nc_id
                numDims `shouldBe` (fromIntegral $ natVal (Proxy :: Proxy n))
            it "correctly returns variable rank - scalar" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "scalar_var"
                numDims                <- checkNC =<< nc_inq_varndims nc_id var
                _                      <- checkNC =<< nc_close nc_id
                numDims `shouldBe` (fromIntegral $ natVal (Proxy :: Proxy n))
        context "nc_inq_vartype" $ do
            it "correctly returns variable type" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable t  (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "vector_var"
                tv                     <- checkNC =<< nc_inq_vartype nc_id var
                _                      <- checkNC =<< nc_close nc_id
                case t of
                    SNCInt -> tv `shouldBe` (TypedValue NCInt ())
                    _ -> expectationFailure $ "Unexpected variable data type: " ++ show t
        context "nc_inq_varname" $ do
            it "correctly returns variable name" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "scalar_var"
                varName                <- checkNC =<< nc_inq_varname nc_id var
                _                      <- checkNC =<< nc_close nc_id
                varName `shouldBe` "scalar_var"
        context "nc_inq_vardimid" $ do
            it "correctly returns variable dimensions" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable _ (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "var_2d"
                dim1_id                <- checkNC =<< nc_inq_dimid nc_id "dim1"
                dim2_id                <- checkNC =<< nc_inq_dimid nc_id "dim2"
                varDims                <- checkNC =<< nc_inq_vardimid nc_id var
                _                      <- checkNC =<< nc_close nc_id
                ((fromStaticVector <$> varDims) == Just [dim1_id, dim2_id]) `shouldBe` True
                case (Proxy :: Proxy n) of
                    Var2D -> (varDims == Just (D dim1_id :| dim2_id)) `shouldBe` True
                    _     -> expectationFailure $ "Unexpected rank: " ++ (show $ natVal (Proxy :: Proxy n))
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
                deflateParams `shouldBe` (NCDeflateParams True $ Just 0)
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
                (SomeNCVariable _ (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "var_2d"
                chunks                 <- checkNC =<< nc_inq_var_chunking nc_id var
                _                      <- checkNC =<< nc_close nc_id
                (fromStaticVector <$> chunks) `shouldBe` (Just [2, 3])
                case (Proxy :: Proxy n) of
                    Var2D -> chunks `shouldBe` Just (D 2 :| 3)
                    _     -> expectationFailure $ "Unexpected rank: " ++ (show $ natVal (Proxy :: Proxy n))
        context "nc_inq_var_fill" $ do
            it "correctly reports filling value" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_var"
                case t of
                    SNCInt -> do
                        fillValue <- checkNC =<< nc_inq_var_fill nc_id var
                        _         <- checkNC =<< nc_close nc_id
                        fillValue `shouldBe` Just 1
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
                (SomeNCVariable t (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "vector_int"
                case t of
                    SNCInt -> case (Proxy :: Proxy n) of
                        Var1D -> do
                            nc_data <- checkNC =<< nc_get_vara nc_id var (D 0) (D 3)
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` VS.fromList [3, 4, 5]
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a subset of a vector variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "vector_int"
                case t of
                    SNCInt -> case (Proxy :: Proxy n) of
                        Var1D -> do
                            nc_data <- checkNC =<< nc_get_vara nc_id var (D 1) (D 1)
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` VS.fromList [4]
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
        context "nc_get_var1" $ do
            it "correctly reads a single value from vector" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "vector_int"
                case t of
                    SNCInt -> case (Proxy :: Proxy n) of
                        Var1D -> do
                            nc_data <- checkNC =<< nc_get_var1 nc_id var (D 1)
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` 4
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
        context "nc_get_var" $ do
            it "correctly reads a scalar variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t var) <- checkNC =<< nc_inq_varid nc_id "vector_int"
                case t of
                    SNCInt -> do
                        nc_data <- checkNC =<< nc_get_var nc_id var
                        _       <- checkNC =<< nc_close nc_id
                        nc_data `shouldBe` VS.fromList [3,4,5]
                    _ -> expectationFailure "Unexpected data type"
            it "correctly reads a vector variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "scalar_int"
                case t of
                    SNCInt -> do
                        nc_data <- checkNC =<< nc_get_var nc_id var
                        _       <- checkNC =<< nc_close nc_id
                        nc_data `shouldBe` VS.fromList [7]
                    _ -> expectationFailure "Unexpected data type"
        context "nc_get_vars" $ do
            it "correctly reads a vector variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "vector_int"
                case t of
                    SNCInt -> case (Proxy :: Proxy n) of
                        Var1D -> do
                            nc_data <- checkNC =<< nc_get_vars nc_id var (D 0) (D 2) (D 2)
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` VS.fromList [3,5]
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
        context "nc_get_scalar" $ do
            it "correctly reads a scalar variable" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
                (SomeNCVariable t (var :: NCVariableId n a)) <-
                                          checkNC =<< nc_inq_varid nc_id "scalar_int"
                case t of
                    SNCInt -> case (Proxy :: Proxy n) of
                        Var0D -> do
                            nc_data <- checkNC =<< nc_get_scalar nc_id var
                            _       <- checkNC =<< nc_close nc_id
                            nc_data `shouldBe` 7
                        _ -> expectationFailure "Unexpected NC variable rank"
                    _ -> expectationFailure "Unexpected data type"
        context "nc_put_scalar" $ do
            it "correctly writes a scalar variable - 1" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var5_write.nc") NCNetCDF4 NCClobber
                var_id                 <- checkNC =<< nc_def_var' nc_id "variable" NCInt64 ScalarVar
                _                      <- checkNC =<< nc_put_scalar nc_id var_id 7
                v                      <- checkNC =<< nc_get_scalar nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` 7
            it "correctly writes a scalar variable - 2" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "var6_write.nc") NCClassic NCClobber
                var_id                 <- checkNC =<< nc_def_var' nc_id "variable" NCDouble ScalarVar
                _                      <- checkNC =<< nc_enddef nc_id
                _                      <- checkNC =<< nc_put_scalar nc_id var_id 11.5
                v                      <- checkNC =<< nc_get_scalar nc_id var_id
                _                      <- checkNC =<< nc_close nc_id
                v `shouldBe` 11.5
