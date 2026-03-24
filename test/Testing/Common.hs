module Testing.Common where

import           Data.Int (Int32)
import           Foreign.C.Types (CInt)
import           Test.Hspec

check :: HasCallStack => (Int32, b) -> IO b
check (r,v) = shouldNotBe r (-1) >> return v

-- Calls to NetCDF functions return the non-zero
-- error code in case of errors.
checkNC :: HasCallStack => (CInt, b) -> IO b
checkNC (r,v) = shouldBe r 0 >> return v

testOutputPath :: FilePath
testOutputPath = "mohave-test-output"
