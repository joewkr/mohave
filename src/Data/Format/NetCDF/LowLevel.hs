{-# LANGUAGE PatternSynonyms #-}
module Data.Format.NetCDF.LowLevel(
    module Data.Format.NetCDF.LowLevel.C.Definitions
  , module Data.Format.NetCDF.LowLevel.Definitions
  , module Internal.Definitions
) where

import           Data.Format.NetCDF.LowLevel.Definitions
import           Data.Format.NetCDF.LowLevel.C.Definitions ((.|.), queryOpenMode)
import           Internal.Definitions (
                     StaticVector(..)
                   , TypedValue(..)
                   , VarShapeDef(..)
                   , fromStaticVector
                   , pattern Var0D, pattern Var1D, pattern Var2D, pattern Var3D, pattern Var4D, pattern Var5D, pattern Var6D)