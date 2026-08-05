{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Format.HDF.LowLevel.Util(hdfVarNDimsProxy) where

import           Data.Proxy (Proxy(..))
import           Data.Format.HDF.LowLevel.Definitions
import           Data.Format.HDF.LowLevel.SD
import           GHC.TypeLits (Nat, KnownNat)

hdfVarNDimsProxy :: forall a (t :: HDataType a) (n :: Nat). KnownNat n => SDataSetId n t -> Proxy n
hdfVarNDimsProxy _ = Proxy

