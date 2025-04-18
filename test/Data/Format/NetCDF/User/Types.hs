{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Format.NetCDF.User.Types where

import           Data.Int (Int32)
import           Foreign.Ptr (castPtr, plusPtr)
import           Foreign.Storable

import           Data.Format.NetCDF.LowLevel

data Compound = Compound Int32 Float Float deriving (Eq, Show)

instance Storable Compound where
  alignment _ = 8
  sizeOf _ = 12
  poke ptr (Compound i f1 f2) = do
    poke (castPtr ptr) i
    poke (castPtr $ plusPtr ptr 4) f1
    poke (castPtr $ plusPtr ptr 8) f2
  peek ptr = do
    i  <- peek (castPtr ptr)
    f1 <- peek (castPtr $ plusPtr ptr 4)
    f2 <- peek (castPtr $ plusPtr ptr 8)
    return $ Compound i f1 f2

type instance EquivalentHaskellType (TNCCompound '[ '( 'TNCInt, 0), '( 'TNCFloat, 4), '( 'TNCFloat, 8)]) = Compound

-- Custom type instance for writing (Compound i _ f), i.e. skipping the second field, to a NetCDF file
type instance EquivalentHaskellType (TNCCompound '[ '( 'TNCInt, 0), '( 'TNCFloat, 8)]) = Compound

pattern SCompound :: forall (t :: NCDataTypeTag). () => (t ~ (TNCCompound '[ '( 'TNCInt, 0), '( 'TNCFloat, 4), '( 'TNCFloat, 8)])) => NCDataTypeTagS t
pattern SCompound <- SNCCompound SNCInt [snat3|0|] (SNCCompound SNCFloat [snat3|4|] (SNCCompound SNCFloat [snat3|8|] SNCCompoundE))

data CompoundSparseA = CompoundSparseA Float Int32 deriving (Eq, Show)
instance Storable CompoundSparseA where
  alignment _ = 8
  sizeOf _ = 8
  poke ptr (CompoundSparseA f2 i) = do
    poke (castPtr ptr) f2
    poke (castPtr $ plusPtr ptr 4) i
  peek ptr = do
    (f2 :: Float) <- peek (castPtr ptr)
    (i  :: Int32) <- peek (castPtr $ plusPtr ptr 4)
    return $ CompoundSparseA f2 i

type instance EquivalentHaskellType (TNCCompound '[ '( 'TNCFloat, 0), '( 'TNCInt, 4)]) = CompoundSparseA

pattern SCompoundSparseA :: forall (t :: NCDataTypeTag). () => (t ~ (TNCCompound '[ '( 'TNCFloat, 0), '( 'TNCInt, 4)])) => NCDataTypeTagS t
pattern SCompoundSparseA <- SNCCompound SNCFloat [snat3|0|] (SNCCompound SNCInt [snat3|4|] SNCCompoundE)


data CompoundAttr = CompoundAttr (EquivalentHaskellType TNCUInt) (EquivalentHaskellType TNCUByte) deriving (Show, Eq)
instance Storable CompoundAttr where
  alignment _ = 8
  sizeOf _ = 8
  poke ptr (CompoundAttr i1 i2) = do
    poke (castPtr ptr) i1
    poke (castPtr $ plusPtr ptr 4) i2
  peek ptr = do
    i1  <- peek (castPtr ptr)
    i2 <- peek (castPtr $ plusPtr ptr 4)
    return $ CompoundAttr i1 i2

type instance EquivalentHaskellType (TNCCompound '[ '( 'TNCUInt, 0), '( 'TNCUByte, 4)]) = CompoundAttr

pattern SCompoundAttr :: forall (t :: NCDataTypeTag). () => (t ~ (TNCCompound '[ '( 'TNCUInt, 0), '( 'TNCUByte, 4)])) => NCDataTypeTagS t
pattern SCompoundAttr <- SNCCompound SNCUInt [snat3|0|] (SNCCompound SNCUByte [snat3|4|] SNCCompoundE)

data CompoundWithComment = CompoundWithComment {
    number  :: (EquivalentHaskellType TNCUInt)
  , comment :: (EquivalentHaskellType TNCString)
} deriving (Show, Eq)
instance Storable CompoundWithComment where
  alignment _ = 8
  sizeOf _ = 16
  poke ptr (CompoundWithComment n str) = do
    poke (castPtr ptr) n
    poke (castPtr $ plusPtr ptr 8) str
  peek ptr = do
    n  <- peek (castPtr ptr)
    str <- peek (castPtr $ plusPtr ptr 8)
    return $ CompoundWithComment n str

type instance EquivalentHaskellType (TNCCompound '[ '( 'TNCInt, 0), '( 'TNCString, 8)]) = CompoundWithComment

pattern SCompoundWithComment :: forall (t :: NCDataTypeTag). () => (t ~ (TNCCompound '[ '( 'TNCInt, 0), '( 'TNCString, 8)])) => NCDataTypeTagS t
pattern SCompoundWithComment <- SNCCompound SNCInt [snat3|0|] (SNCCompound SNCString [snat3|8|] SNCCompoundE)
