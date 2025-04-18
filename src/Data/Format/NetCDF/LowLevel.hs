{-# LANGUAGE PatternSynonyms #-}
module Data.Format.NetCDF.LowLevel(
    module Data.Format.NetCDF.LowLevel.C.Definitions
  , module Data.Format.NetCDF.LowLevel.Definitions
  , module Internal.Definitions
  , module Internal.Numerals.Ternary
) where

import           Data.Format.NetCDF.LowLevel.Definitions
import           Data.Format.NetCDF.LowLevel.C.Definitions (
                      (.|.)
                    , queryOpenMode
                    , pattern NCByte
                    , pattern NCUByte
                    , pattern NCChar
                    , pattern NCShort
                    , pattern NCUShort
                    , pattern NCInt
                    , pattern NCUInt
                    , pattern NCInt64
                    , pattern NCUInt64
                    , pattern NCFloat
                    , pattern NCDouble
                    , pattern NCString )
import           Internal.Definitions (
                     StaticVector(..)
                   , TypedValue(..)
                   , fromStaticVector
                   , pattern Var0D, pattern Var1D, pattern Var2D, pattern Var3D, pattern Var4D, pattern Var5D, pattern Var6D)
import           Internal.Numerals.Ternary
