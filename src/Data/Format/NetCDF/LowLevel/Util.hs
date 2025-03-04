module Data.Format.NetCDF.LowLevel.Util(checkNC) where

import           Data.Int (Int32)
import           Data.Format.NetCDF.LowLevel.Error
import           GHC.Stack (HasCallStack)

checkNC ::  HasCallStack => (Int32, a) -> IO a
checkNC (0,v) = return v
checkNC (e,_) = nc_strerror (fromNCErrorCode $ fromIntegral e) >>= error

