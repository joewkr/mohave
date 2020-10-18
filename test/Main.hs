module Main where

import           Test.Hspec
import           System.Directory

import qualified Spec
import           Testing.Common

main :: IO ()
main = hspec (beforeAll_ prep Spec.spec)

prep :: IO ()
prep = do
    haveOldOutput <- doesDirectoryExist testOutputPath
    if haveOldOutput
        then removeDirectoryRecursive testOutputPath
        else return ()
    createDirectory testOutputPath
