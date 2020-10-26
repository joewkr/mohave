module Main (main) where

import           Data.Char (isDigit, toUpper)
import           Data.Version (Version)
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.Program
import           Distribution.Verbosity (normal)

main :: IO ()
main =  do
    cppOpts <- getPkgConfigVersionDefs ["netcdf", "hdf"]
    defaultMainWithHooks $ addPkgConfigVersionDefs ("-DPKG_VERSION(a,b,c)=(a*65025 + b*255 + c)":cppOpts) simpleUserHooks

getPkgConfigVersionDefs :: [String] -> IO [String]
getPkgConfigVersionDefs = mapM pkgConfigVersion

pkgConfigVersion :: String -> IO String
pkgConfigVersion moduleName = do
    pkgDb <- configureProgram normal pkgConfigProgram defaultProgramDb
    pkgConfigOutput <- getDbProgramOutput normal pkgConfigProgram pkgDb ["--modversion", moduleName]
    let versionNumber =  combineMMP . getMMP . takeWhile (\c -> isDigit c || c == '.') $ init pkgConfigOutput
    return $! "-DPKG_CONFIG_" ++ (map toUpper moduleName) ++ "_VERSION=" ++ (show versionNumber)

-- Parse a version string assuming the Major.Minor.Patch format
getMMP :: String -> (Int, Int, Int)
getMMP version = (read major, read minor, read path)
    where
        (major, minorPath) = span (/= '.') version
        (minor, couldBePath) = if null minorPath
            then ("0", "")
            else span (/= '.') (tail minorPath)
        path = if null couldBePath then "0" else (fst . span (/= '.') $ tail couldBePath)

combineMMP :: (Int, Int, Int) -> Int
combineMMP (major, minor, path) = major*255^2 + minor*255 + path

addPkgConfigVersionDefs :: [String] -> UserHooks -> UserHooks
addPkgConfigVersionDefs cppOpts oldUserHooks = oldUserHooks{
    preBuild = pkgConfigHook cppOpts $ preBuild oldUserHooks
  , preRepl  = pkgConfigHook cppOpts $ preRepl  oldUserHooks}

pkgConfigHook :: [String] -> (args -> flags -> IO HookedBuildInfo) -> args -> flags -> IO HookedBuildInfo
pkgConfigHook cppOpts oldFunc args flags = do
  (maybeOldLibHookedInfo, oldExeHookedInfo) <- oldFunc args flags
  case maybeOldLibHookedInfo of
    Just oldLibHookedInfo -> do
      let newLibHookedInfo = oldLibHookedInfo {cppOptions = cppOptions oldLibHookedInfo <> cppOpts}
      return (Just newLibHookedInfo, oldExeHookedInfo)
    Nothing -> do
      let newLibHookedInfo = emptyBuildInfo{cppOptions = cppOpts}
      return (Just newLibHookedInfo, oldExeHookedInfo)
