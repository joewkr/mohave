module Data.Format.HDF.LowLevel.HESpec(spec) where

import           Data.Format.HDF.LowLevel.HE
import           Data.Format.HDF.LowLevel.SD
import           Data.Int
import           Foreign.C.Types
import           System.FilePath ((</>))
import           Test.Hspec
import           Testing.Common

spec :: Spec
spec = do
    describe "HDF error reporting" $ do
        it "dumps HDF errors to file" $ do
            let logFileName = testOutputPath </> "hdf-error-trace.log"
                expectedFirstLine = "HDF error: (5) <Bad file name on open>"
            (status_1, _) <-           sd_start "test-data/sd/NULL" HDFRead
            _             <-           he_print logFileName
            firstLine     <- (head . lines) <$> readFile logFileName
            status_1 `shouldBe` (-1)
            firstLine `shouldBe` expectedFirstLine
        it "produces an empty error dump file when no errors reported" $ do
            let logFileName = testOutputPath </> "hdf-error-trace.no-errors.log"
            sd_id         <- check =<< sd_start "test-data/sd/test1.hdf" HDFRead
            _             <- check =<< sd_end sd_id
            _             <-           he_print logFileName
            contents      <-           readFile logFileName
            contents `shouldBe` []
        it "reports HDF error code" $ do
            (status_1, _) <-           sd_start "test-data/sd/NULL" HDFRead
            he            <-           he_value 1
            status_1 `shouldBe` (-1)
            he       `shouldBe` DFE_BADNAME
        it "converts HDF error code to string" $ do
            he_string DFE_NONE          `shouldReturn` "No error"
            he_string DFE_FNF           `shouldReturn` "File not found"
            he_string DFE_DENIED        `shouldReturn` "Access to file denied"

            he_string DFE_SDS_NOTFOUND  `shouldReturn` "Unable to find SDS with corresponding name"

            he_string (DFE_UNKNOWN_ERROR 999) `shouldReturn` "Unknown error"
        it "handles adding custom message to the error" $ do
            let logFileName = testOutputPath </> "hdf-error-trace-custom.log"
                expectedFirstLine = "\tCustom HDF error 177"
            (status_1, _) <-           sd_start "test-data/sd/NULL" HDFRead
            _             <-           he_report "Custom HDF error %i" (177 :: Int32)
            _             <-           he_print logFileName
            hdfErrors     <- (take 3 . lines) <$> readFile logFileName
            status_1 `shouldBe` (-1)
            (last hdfErrors) `shouldBe` expectedFirstLine
        it "clears HDF error stack" $ do
            let logFileName = testOutputPath </> "hdf-error-trace-cleared.log"
            (status_1, _) <-           sd_start "test-data/sd/NULL" HDFRead
            _             <-           he_clear
            _             <-           he_print logFileName
            contents      <-           readFile logFileName
            status_1 `shouldBe` (-1)
            contents `shouldBe` []
        it "builds custom HDF error stack" $ do
            let logFileName = testOutputPath </> "hdf-error-trace-custom-2.log"
                expectedTrace = "HDF error: (2) <Access to file denied>\n\
                                \\tDetected in spec() [HESpec.hs line 1122]\n\
                                \\tCustom HDF error report 1.23\n"
            _             <-           he_clear
            _             <-           he_push DFE_DENIED "spec" "HESpec.hs" 1122
            _             <-           he_report "Custom HDF error report %4.2f" (1.23 :: CDouble)
            _             <-           he_print logFileName
            contents      <-           readFile logFileName
            contents `shouldBe` expectedTrace
        {-
        -- Non-deterministic test, expects that if memory storing the file name has been
        -- freed too early it could be overwritten and result in corrupted output. When this
        -- problem was not fixed yet it could take a few thousand iterations to generate
        -- corrupted output.
        --
        -- Requires:
        -- import           Control.Monad (forM_)
        -- import           Foreign.C.String (newCString)
        -- import           System.Mem (performGC)

        it "keeps correct HDF error stack after GC, 10000 iterations" $ do
            let logFileName = "/dev/shm/hdf-error-trace-custom-3.log"
            forM_ [1..10000] $ \index -> do
                let expectedTrace = "HDF error: (2) <Access to file denied>\n\
                            \\tDetected in spec() [HESpec.hs" ++ (show index) ++ " line 1122]\n\
                            \\tCustom HDF error report 1.23\n"
                _             <-           he_clear
                _             <-           he_push DFE_DENIED "aaa" ("HESpec.hs" ++ show (index :: Int)) 11
                _             <-           he_push DFE_DENIED "bbb" ("HESpec.hs" ++ show (index :: Int)) 22
                _             <-           he_push DFE_DENIED "ccc" ("HESpec.hs" ++ show (index :: Int)) 33
                _             <-           he_push DFE_DENIED "ddd" ("HESpec.hs" ++ show (index :: Int)) 44
                _             <-           he_push DFE_DENIED "eee" ("HESpec.hs" ++ show (index :: Int)) 55
                _             <-           he_push DFE_DENIED "fff" ("HESpec.hs" ++ show (index :: Int)) 66
                _             <-           he_push DFE_DENIED "ggg" ("HESpec.hs" ++ show (index :: Int)) 77
                _             <-           he_push DFE_DENIED "hhh" ("HESpec.hs" ++ show (index :: Int)) 88
                _             <-           he_push DFE_DENIED "iii" ("HESpec.hs" ++ show (index :: Int)) 99
                _             <-           sd_start "NULL" HDFRead
                _             <-           he_push DFE_DENIED "spec" ("HESpec.hs" ++ show (index :: Int)) 1122
                _             <-           he_report "Custom HDF error report %4.2f" (1.23 :: CDouble)
                _             <-           performGC
                _             <-           newCString $ replicate 4096 'x'
                _             <-           he_print logFileName
                contents      <- (unlines . take 3 . lines) <$> readFile logFileName
                contents `shouldBe` expectedTrace
        -}

