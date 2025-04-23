{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Format.NetCDF.LowLevel.UserTypeSpec(spec) where

import           Control.Monad (void, forM_)
import           Data.Int (Int32)
import           Data.Word (Word32)
import           System.FilePath ((</>))
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)

import           Data.Format.NetCDF.LowLevel
import           Data.Format.NetCDF.LowLevel.File
import           Data.Format.NetCDF.LowLevel.Dimension
import           Data.Format.NetCDF.LowLevel.Variable
import           Data.Format.NetCDF.LowLevel.User.Type
import           Testing.Common

data Compound = Compound Int32 Float Float deriving (Eq, Show)

spec :: Spec
spec = do
  describe "Ternary numerals" $ do
    modifyMaxSuccess (const 1000) $ prop "properly creates ternary numerals" $ \x ->
      case toTernarySNat $ fromIntegral x of
        SomeTernarySNat n -> (fromIntegral $ fromTernarySNat n) `shouldBe` (x :: Word32)
    it "creates correct ternary SNat with QuasiQuoter - 1" $
      case toTernarySNat 0 of
        SomeTernarySNat [snat3|0|] -> return ()
        _ -> expectationFailure "Mismatch between runtime and QQ TernarySNat"
    it "creates correct ternary SNat with QuasiQuoter - 2" $
      case toTernarySNat 111 of
        SomeTernarySNat [snat3|111|] -> return ()
        _ -> expectationFailure "Mismatch between runtime and QQ TernarySNat"
    it "creates correct ternary SNat with QuasiQuoter - 3" $
      case toTernarySNat 27 of
        SomeTernarySNat [snat3|27|] -> return ()
        _ -> expectationFailure "Mismatch between runtime and QQ TernarySNat"
  describe "Compound types" $ do
    context "nc_def_compound" $ do
      it "correctly defines a compound type" $ do
        nc_id                  <- checkNC =<< nc_create (testOutputPath </> "compound_type1.nc") NCNetCDF4 NCClobber
        type_id0               <- checkNC =<< nc_def_compound nc_id 8 "test_compound"
        type_id1               <- checkNC =<< nc_insert_compound nc_id type_id0 "field1" (ST0 STBot) NCInt
        type_id2               <- checkNC =<< nc_insert_compound nc_id type_id1 "field2" (ST1 (ST1 STBot)) NCFloat
        dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 10)
        _                      <- checkNC =<< nc_def_var nc_id "compound_variable" type_id2 (D dim_id :| dim_id)
        _                      <- checkNC =<< nc_enddef nc_id
        void $                    checkNC =<< nc_close nc_id
    context "nc_insert_compound" $ do
      it "correctly inserts atomic field" $ do
        nc_id                  <- checkNC =<< nc_create (testOutputPath </> "compound_type2.nc") NCNetCDF4 NCClobber
        type_id0               <- checkNC =<< nc_def_compound nc_id 4 "test_compound"
        void $                    checkNC =<< nc_insert_compound nc_id type_id0 "field1" (ST0 STBot) NCInt
        void $                    checkNC =<< nc_close nc_id
      it "correctly inserts atomic field - custom alignment" $ do
        nc_id                  <- checkNC =<< nc_create (testOutputPath </> "compound_type2.nc") NCNetCDF4 NCClobber
        type_id0               <- checkNC =<< nc_def_compound nc_id 5 "test_compound"
        void $                    checkNC =<< nc_insert_compound nc_id type_id0 "field1" (ST1 STBot) NCInt
        void $                    checkNC =<< nc_close nc_id
      it "correctly inserts compound field" $ do
        nc_id                  <- checkNC =<< nc_create (testOutputPath </> "compound_type3.nc") NCNetCDF4 NCClobber
        inner_type_id0         <- checkNC =<< nc_def_compound nc_id 8 "inner_compound"
        inner_type_id1         <- checkNC =<< nc_insert_compound nc_id inner_type_id0 "inner1" (ST0 STBot) NCByte
        inner_type_id2         <- checkNC =<< nc_insert_compound nc_id inner_type_id1 "inner2" (ST1 STBot) NCByte
        inner_type_id3         <- checkNC =<< nc_insert_compound nc_id inner_type_id2 "inner3" (ST2 STBot) NCUShort
        inner_type_id4         <- checkNC =<< nc_insert_compound nc_id inner_type_id3 "inner4" (ST1 (ST1 STBot)) NCFloat

        type_id0               <- checkNC =<< nc_def_compound nc_id 12 "test compound"
        type_id1               <- checkNC =<< nc_insert_compound nc_id type_id0 "field1" (ST0 STBot) NCInt
        type_id2               <- checkNC =<< nc_insert_compound nc_id type_id1 "field2" (ST1 (ST1 STBot)) inner_type_id4
        dim_id                 <- checkNC =<< nc_def_dim nc_id "nc_dimension" (Just 3)
        _                      <- checkNC =<< nc_def_var nc_id "compound_variable" type_id2 (D dim_id :| dim_id)
        _                      <- checkNC =<< nc_enddef nc_id
        void $                    checkNC =<< nc_close nc_id
    context "nc_inq_compound" $ do
      it "correctly retrieves compound type info" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "compound_t"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            typeInfo <- checkNC =<< nc_inq_compound nc_id t
            typeSize <- checkNC =<< nc_inq_compound_size nc_id t
            typeInfo `shouldBe` (NCCompoundTypeInfo "compound_t" 3 typeSize)
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
    context "nc_inq_compound_name" $ do
      it "correctly retrieves compound type name" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "compound_t"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            typeName <- checkNC =<< nc_inq_compound_name nc_id t
            typeName `shouldBe` "compound_t"
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
    context "nc_inq_compound_size" $ do
      it "correctly retrieves compound type size" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "compound_t"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            typeSize <- checkNC =<< nc_inq_compound_size nc_id t
            typeSize `shouldBe` 12
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
      it "correctly retrieves compound type size - packed" $ do
        -- It seems that NetCDF (HDF5) always reverts to native allignment/padding
        -- when reading a compound type even if an "__attribute__((packed))" struct
        -- was used for defining and writing a compound variable (which also results
        -- in a smaller NetCDF file, so packing is respected on the low level).
        nc_id                  <- checkNC =<< nc_open "test-data/nc/compound.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "S"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            typeSize <- checkNC =<< nc_inq_compound_size nc_id t
            typeSize `shouldBe` 16
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
      it "correctly retrieves compound type size - unpacked" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/compound.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "D"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            typeSize <- checkNC =<< nc_inq_compound_size nc_id t
            typeSize `shouldBe` 16
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
    context "nc_inq_compound_nfields" $ do
      it "correctly retrieves number of fields" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "compound_t"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            numfFields <- checkNC =<< nc_inq_compound_nfields nc_id t
            numfFields `shouldBe` 3
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
    context "nc_inq_compound_field" $ do
      it "correctly retrieves compoind field info - 1" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "compound_t"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            fieldInfo <- checkNC =<< nc_inq_compound_field nc_id t 0
            (ncCompoundFieldName fieldInfo) `shouldBe` "n"
            (ncCompoundFieldOffset fieldInfo) `shouldBe` 0
            (ncCompoundFieldDims   fieldInfo) `shouldBe` []
            case ncCompoundFieldType fieldInfo of
              SomeNCType NCInt -> return ()
              _ -> expectationFailure $ "Unexpected fieldType type"
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
      it "correctly retrieves compoind field info - 2" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "compound_t"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            fieldInfo        <- checkNC =<< nc_inq_compound_field nc_id t 1
            fieldOffset      <- checkNC =<< nc_inq_compound_fieldoffset nc_id t 1
            (SomeNCType ft2) <- checkNC =<< nc_inq_compound_fieldtype nc_id t 1
            (ncCompoundFieldName   fieldInfo) `shouldBe` "x field name"
            (ncCompoundFieldOffset fieldInfo) `shouldBe` fieldOffset
            (ncCompoundFieldDims   fieldInfo) `shouldBe` []
            case ncCompoundFieldType fieldInfo of
              (SomeNCType ft1@NCType{ncTypeTag=SNCFloat}) -> do
                equal <- checkNC =<< nc_inq_type_equal nc_id ft1 nc_id ft2
                equal `shouldBe` True
              _ -> expectationFailure $ "Unexpected fieldType type"
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
    context "nc_inq_compound_fieldname" $ do
      it "returns correct field name - 1" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/compound.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "S"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            fieldName <- checkNC =<< nc_inq_compound_fieldname nc_id t 0
            fieldName `shouldBe` "c"
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
      it "returns correct field name - 2" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/compound.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "D"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            fieldName <- checkNC =<< nc_inq_compound_fieldname nc_id t 0
            fieldName `shouldBe` "d"
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
      it "returns correct field name - 3" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "compound_t"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            fieldName <- checkNC =<< nc_inq_compound_fieldname nc_id t 1
            fieldName `shouldBe` "x field name"
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
    context "nc_inq_compound_fieldoffset" $ do
      it "returns correct field offset - 1" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/compound.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "S"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            fieldIdx1 <- checkNC =<< nc_inq_compound_fieldindex nc_id t "c"
            offset <- checkNC =<< nc_inq_compound_fieldoffset nc_id t fieldIdx1
            offset `shouldBe` 0
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
      it "returns correct field offset - 2" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/compound.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "D"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            fieldIdx1 <- checkNC =<< nc_inq_compound_fieldindex nc_id t "c"
            offset <- checkNC =<< nc_inq_compound_fieldoffset nc_id t fieldIdx1
            offset `shouldSatisfy` (> 0)
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
    context "nc_inq_compound_fieldtype" $ do
      it "retruns correct field type - 1" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "c3"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            fieldIdx1 <- checkNC =<< nc_inq_compound_fieldindex nc_id t "arr"
            (SomeNCType fieldType) <- checkNC =<< nc_inq_compound_fieldtype nc_id t fieldIdx1
            equal <- checkNC =<< nc_inq_type_equal nc_id fieldType nc_id NCFloat
            equal `shouldBe` True
            case fieldType of
              NCType{ncTypeTag=SNCFloat} -> return ()
              _ -> expectationFailure $ "Unexpected field type:\t" ++ show (ncTypeTag fieldType)
          _ -> expectationFailure $ "Unexpected data type:\t" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
      it "retruns correct field type - 2" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "c3"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            fieldIdx1 <- checkNC =<< nc_inq_compound_fieldindex nc_id t "aa"
            (SomeNCType fieldType) <- checkNC =<< nc_inq_compound_fieldtype nc_id t fieldIdx1
            equal <- checkNC =<< nc_inq_type_equal nc_id fieldType nc_id NCInt
            equal `shouldBe` True
            case fieldType of
              NCType{ncTypeTag=SNCInt} -> return ()
              _ -> expectationFailure $ "Unexpected field type:\t" ++ show (ncTypeTag fieldType)
          _ -> expectationFailure $ "Unexpected data type:\t" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
      it "retruns correct field type - 3" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "c3"
        (SomeNCType t2)        <- checkNC =<< nc_inq_typeid nc_id "c2"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            fieldIdx1 <- checkNC =<< nc_inq_compound_fieldindex nc_id t "scalar_c2"
            (SomeNCType fieldType) <- checkNC =<< nc_inq_compound_fieldtype nc_id t fieldIdx1
            equal <- checkNC =<< nc_inq_type_equal nc_id fieldType nc_id t2
            equal `shouldBe` True
            case fieldType of
              NCType{ncTypeTag=SNCCompound{}} -> return ()
              _ -> expectationFailure $ "Unexpected field type:\t" ++ show (ncTypeTag fieldType)
          _ -> expectationFailure $ "Unexpected data type:\t" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
    context "nc_inq_compound_fieldndims" $ do
      it "correctly returns field rank - 1" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "c3"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            fieldRank <- checkNC =<< nc_inq_compound_fieldndims nc_id t 2
            fieldRank `shouldBe` 3
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
      it "correctly returns field rank - 2" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "c3"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            fieldRank <- checkNC =<< nc_inq_compound_fieldndims nc_id t 1
            fieldRank `shouldBe` 0
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
    context "nc_inq_compound_fielddim_sizes" $ do
      it "correctly returns field dim sizes - 1" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "c3"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            fieldRank <- checkNC =<< nc_inq_compound_fielddim_sizes nc_id t 2
            fieldRank `shouldBe` [4,8,15]
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
      it "correctly returns field dim sizes - 2" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "c3"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            fieldRank <- checkNC =<< nc_inq_compound_fielddim_sizes nc_id t 1
            fieldRank `shouldBe` []
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
    context "nc_inq_compound_fieldindex" $ do
      it "returns correct field index - 1" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/compound.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "S"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            fieldIdx1 <- checkNC =<< nc_inq_compound_fieldindex nc_id t "c"
            fieldIdx2 <- checkNC =<< nc_inq_compound_fieldindex nc_id t "d"
            [fieldIdx1,fieldIdx2] `shouldBe` [0,1]
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
      it "returns correct field index - 2" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/compound.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "D"
        case t of
          NCType{ncTypeTag=SNCCompound{}} -> do
            fieldIdx1 <- checkNC =<< nc_inq_compound_fieldindex nc_id t "c"
            fieldIdx2 <- checkNC =<< nc_inq_compound_fieldindex nc_id t "d"
            [fieldIdx1,fieldIdx2] `shouldBe` [1,0]
          _ -> expectationFailure $ "Unexpected data type" ++ show (ncTypeTag t)
        void $                    checkNC =<< nc_close nc_id
    context "nc_inq_type_equal" $ do
      it "correctly compares atomic types - 1" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
        forM_ [
            SomeNCType NCByte
          , SomeNCType NCUByte
          , SomeNCType NCChar
          , SomeNCType NCShort
          , SomeNCType NCUShort
          , SomeNCType NCInt
          , SomeNCType NCUInt
          , SomeNCType NCInt64
          , SomeNCType NCUInt64
          , SomeNCType NCFloat
          , SomeNCType NCDouble
          , SomeNCType NCString] $ \(SomeNCType t) -> do
          equal                  <- checkNC =<< nc_inq_type_equal nc_id t nc_id t
          equal `shouldBe` True
        void $                    checkNC =<< nc_close nc_id
      it "correctly compares atomic types - 2" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
        equal                  <- checkNC =<< nc_inq_type_equal nc_id NCInt nc_id NCUInt
        void $                    checkNC =<< nc_close nc_id
        equal `shouldBe` False
      it "correctly compares atomic types - 3" $ do
        nc_id1                 <- checkNC =<< nc_open "test-data/nc/test1.nc" NCNoWrite
        nc_id2                 <- checkNC =<< nc_open "test-data/nc/test2.nc" NCNoWrite
        equal                  <- checkNC =<< nc_inq_type_equal nc_id1 NCInt nc_id2 NCInt
        void $                    checkNC =<< nc_close nc_id1
        void $                    checkNC =<< nc_close nc_id2
        equal `shouldBe` True
      it "correctly compares compound types - 1" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "compound_t"
        equal                  <- checkNC =<< nc_inq_type_equal nc_id t nc_id t
        void $                    checkNC =<< nc_close nc_id
        equal `shouldBe` True
      it "correctly compares compound types - 2" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
        (SomeNCType t)         <- checkNC =<< nc_inq_typeid nc_id "compound_t"
        equal                  <- checkNC =<< nc_inq_type_equal nc_id t nc_id NCFloat
        void $                    checkNC =<< nc_close nc_id
        equal `shouldBe` False
      it "correctly compares compound types - 3" $ do
        nc_id1                 <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
        (SomeNCType t1)        <- checkNC =<< nc_inq_typeid nc_id1 "c1"
        nc_id2                 <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
        (SomeNCType t2)        <- checkNC =<< nc_inq_typeid nc_id2 "c1"
        equal                  <- checkNC =<< nc_inq_type_equal nc_id1 t1 nc_id2 t2
        void $                    checkNC =<< nc_close nc_id1
        void $                    checkNC =<< nc_close nc_id2
        equal `shouldBe` True
      it "correctly compares compound types - 4" $ do
        nc_id1                 <- checkNC =<< nc_open "test-data/nc/test3.nc" NCNoWrite
        (SomeNCType t1)        <- checkNC =<< nc_inq_typeid nc_id1 "c1"
        nc_id2                 <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
        (SomeNCType t2)        <- checkNC =<< nc_inq_typeid nc_id2 "c2"
        equal                  <- checkNC =<< nc_inq_type_equal nc_id1 t1 nc_id2 t2
        void $                    checkNC =<< nc_close nc_id1
        void $                    checkNC =<< nc_close nc_id2
        equal `shouldBe` True
      it "correctly compares compound types - 5" $ do
        nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
        (SomeNCType t1)        <- checkNC =<< nc_inq_typeid nc_id "c2"
        (SomeNCType t2)        <- checkNC =<< nc_inq_typeid nc_id "c3"
        equal                  <- checkNC =<< nc_inq_type_equal nc_id t1 nc_id t2
        void $                    checkNC =<< nc_close nc_id
        equal `shouldBe` False
