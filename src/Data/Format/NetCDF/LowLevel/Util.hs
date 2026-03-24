{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Format.NetCDF.LowLevel.Util(checkNC, ncVarNDimsProxy) where

import           Data.Proxy (Proxy(..))
import           Data.Format.NetCDF.LowLevel
import           Data.Format.NetCDF.LowLevel.Error
import           Foreign.C.Types (CInt)
import           GHC.Stack (HasCallStack)
import           GHC.TypeLits (Nat, KnownNat)

checkNC ::  HasCallStack => (CInt, a) -> IO a
checkNC (0,v) = return v
checkNC (e,_) = nc_strerror (fromNCErrorCode e) >>= error

ncVarNDimsProxy :: forall (t :: NCDataTypeTag) (n :: Nat). KnownNat n => NCVariableId n t -> Proxy n
ncVarNDimsProxy _ = Proxy
