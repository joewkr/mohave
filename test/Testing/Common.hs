module Testing.Common where

import           Data.Int (Int32)
import           Test.Hspec

check :: HasCallStack => (Int32, b) -> IO b
check (r,v) = shouldNotBe r (-1) >> return v

testOutputPath :: FilePath
testOutputPath = "mohave-test-output"
