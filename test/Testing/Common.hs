module Testing.Common where

import           Data.Int (Int32)
import           Test.Hspec

check :: HasCallStack => (Int32, b) -> IO b
check (r,v) = shouldNotBe r (-1) >> return v

-- Calls to NetCDF functions return the non-zero
-- error code in case of errors.
checkNC :: HasCallStack => (Int32, b) -> IO b
checkNC (r,v) = shouldBe r 0 >> return v

testOutputPath :: FilePath
testOutputPath = "mohave-test-output"
