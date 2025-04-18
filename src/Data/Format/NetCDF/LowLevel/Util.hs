{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Format.NetCDF.LowLevel.Util(checkNC, ncVarNDimsProxy) where

import           Data.Int (Int32)
import           Data.Proxy (Proxy(..))
import           Data.Format.NetCDF.LowLevel
import           Data.Format.NetCDF.LowLevel.Error
import           GHC.Stack (HasCallStack)
import           GHC.TypeLits (Nat, KnownNat)

checkNC ::  HasCallStack => (Int32, a) -> IO a
checkNC (0,v) = return v
checkNC (e,_) = nc_strerror (fromNCErrorCode $ fromIntegral e) >>= error

ncVarNDimsProxy :: forall (t :: NCDataTypeTag) (n :: Nat). KnownNat n => NCVariableId n t -> Proxy n
ncVarNDimsProxy _ = Proxy
