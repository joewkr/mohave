module Data.Format.NetCDF.LowLevel.GroupSpec(spec) where

import           Control.Monad (mapM)
import           Test.Hspec
import           System.FilePath ((</>))

import           Data.Format.NetCDF.LowLevel
import           Data.Format.NetCDF.LowLevel.File
import           Data.Format.NetCDF.LowLevel.Group
import           Data.Format.NetCDF.LowLevel.Variable
import           Testing.Common

spec :: Spec
spec = do
    describe "Groups" $ do
        context "nc_def_grp" $ do
            it "correctly creates a new group" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "group1.nc") NCNetCDF4 NCClobber
                gr_id1                 <- checkNC =<< nc_def_grp nc_id "integers"
                _                      <- checkNC =<< nc_def_scalar_var gr_id1 "variable" NCInt
                gr_id2                 <- checkNC =<< nc_def_grp gr_id1 "sub_group"
                _                      <- checkNC =<< nc_def_scalar_var gr_id2 "variable" NCInt64
                _                      <- checkNC =<< nc_close nc_id
                return ()
        context "nc_inq_dimids" $ do
            it "returns corrects group dimensions - 1" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                dims                   <- checkNC =<< nc_inq_dimids nc_id False
                _                      <- checkNC =<< nc_close nc_id
                length dims `shouldBe` 2
            it "returns corrects group dimensions - 2" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                gr_id                  <- checkNC =<< nc_inq_ncid nc_id "vectors"
                dims                   <- checkNC =<< nc_inq_dimids gr_id False
                _                      <- checkNC =<< nc_close nc_id
                length dims `shouldBe` 1
            it "returns corrects group dimensions - 3" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                gr_id                  <- checkNC =<< nc_inq_ncid nc_id "vectors"
                dims                   <- checkNC =<< nc_inq_dimids gr_id True
                _                      <- checkNC =<< nc_close nc_id
                length dims `shouldBe` 3
        context "nc_inq_ncid" $ do
            it "returns group id" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                _                      <- checkNC =<< nc_inq_ncid nc_id "vectors"
                _                      <- checkNC =<< nc_close nc_id
                return ()
        context "nc_inq_grp_full_ncid" $ do
            it "returns group id" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                _                      <- checkNC =<< nc_inq_grp_full_ncid nc_id "/vectors/nested"
                _                      <- checkNC =<< nc_close nc_id
                return ()
            it "returns root group id" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                _                      <- checkNC =<< nc_inq_grp_full_ncid nc_id "/"
                _                      <- checkNC =<< nc_close nc_id
                return ()
        context "nc_inq_grp_ncid" $ do
            it "gives same result as nc_inq_ncid" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                gr_id1                 <- checkNC =<< nc_inq_ncid nc_id "vectors"
                gr_id2                 <- checkNC =<< nc_inq_grp_ncid nc_id "vectors"
                grpName1               <- checkNC =<< nc_inq_grpname_full gr_id1
                grpName2               <- checkNC =<< nc_inq_grpname_full gr_id2
                _                      <- checkNC =<< nc_close nc_id
                grpName1 `shouldBe` grpName2
        context "nc_inq_grp_parent" $ do
            it "returns correct parent group" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                gr_id1                 <- checkNC =<< nc_inq_grp_full_ncid nc_id "/vectors/nested"
                gr_id                  <- checkNC =<< nc_inq_grp_parent gr_id1
                grpName                <- checkNC =<< nc_inq_grpname gr_id
                _                      <- checkNC =<< nc_close nc_id
                grpName `shouldBe` "vectors"
        context "nc_inq_grpname" $ do
            it "returns correct group name" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                gr_id                  <- checkNC =<< nc_inq_grp_full_ncid nc_id "/vectors/nested"
                grpName                <- checkNC =<< nc_inq_grpname gr_id
                _                      <- checkNC =<< nc_close nc_id
                grpName `shouldBe` "nested"
            it "returns correct root group name" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                gr_id                  <- checkNC =<< nc_inq_grp_full_ncid nc_id "/"
                grpName                <- checkNC =<< nc_inq_grpname gr_id
                _                      <- checkNC =<< nc_close nc_id
                grpName `shouldBe` "/"
        context "nc_inq_grpname_full" $ do
            it "returns correct full group name" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                gr_id                  <- checkNC =<< nc_inq_ncid nc_id "vectors"
                grpName                <- checkNC =<< nc_inq_grpname_full gr_id
                _                      <- checkNC =<< nc_close nc_id
                grpName `shouldBe` "/vectors"
            it "returns correct full root group name" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                gr_id                  <- checkNC =<< nc_inq_grp_full_ncid nc_id "/"
                grpName                <- checkNC =<< nc_inq_grpname_full gr_id
                _                      <- checkNC =<< nc_close nc_id
                grpName `shouldBe` "/"
        context "nc_inq_grpname_len" $ do
            it "returns correct group name length" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                gr_id                  <- checkNC =<< nc_inq_grp_full_ncid nc_id "/vectors/nested"
                len                    <- checkNC =<< nc_inq_grpname_len gr_id
                _                      <- checkNC =<< nc_close nc_id
                len `shouldBe` (length "/vectors/nested")
        context "nc_inq_grps" $ do
            it "returns correct number of subgroups - 1" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                gr_ids                 <- checkNC =<< nc_inq_grps nc_id
                _                      <- checkNC =<< nc_close nc_id
                length gr_ids `shouldBe` 1
            it "returns correct number of subgroups - 2" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                gr_id                  <- checkNC =<< nc_inq_grp_full_ncid nc_id "/vectors"
                gr_ids                 <- checkNC =<< nc_inq_grps gr_id
                grpNames               <- mapM (\g -> checkNC =<< nc_inq_grpname g) gr_ids
                _                      <- checkNC =<< nc_close nc_id
                grpNames `shouldBe` ["nested", "nested2"]
        context "nc_inq_varids" $ do
            it "returns correct number of group variables" $ do
                nc_id                  <- checkNC =<< nc_open "test-data/nc/test5.nc" NCNoWrite
                gr_id                  <- checkNC =<< nc_inq_grp_full_ncid nc_id "/vectors/nested2"
                var_ids                <- checkNC =<< nc_inq_varids gr_id
                length var_ids `shouldBe` 3
        context "nc_rename_grp" $ do
            it "correctly renames a group" $ do
                nc_id                  <- checkNC =<< nc_create (testOutputPath </> "group2.nc") NCNetCDF4 NCClobber
                gr_id1                 <- checkNC =<< nc_def_grp nc_id "integers"
                gr_id2                 <- checkNC =<< nc_def_grp gr_id1 "sub_group"
                _                      <- checkNC =<< nc_def_scalar_var gr_id2 "variable" NCInt64
                _                      <- checkNC =<< nc_rename_grp gr_id2 "renamed"
                grpName                <- checkNC =<< nc_inq_grpname gr_id2
                _                      <- checkNC =<< nc_close nc_id
                grpName `shouldBe` "renamed"
