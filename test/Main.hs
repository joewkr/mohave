module Main where

import           Control.Monad (when)
import           Test.Hspec
import           System.Directory

import qualified Spec
import           Testing.Common

main :: IO ()
main = hspec (beforeAll_ prep Spec.spec)

prep :: IO ()
prep = do
    haveOldOutput <- doesDirectoryExist testOutputPath
    when haveOldOutput $ removeDirectoryRecursive testOutputPath
    createDirectory testOutputPath
