{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Data.Format.NetCDF.LowLevel.VLen(
    nc_get_vara_vlen
  , nc_get_var1_vlen
  , nc_get_var_vlen
  , nc_get_vars_vlen
  , nc_get_vlen
  , nc_put_vlen
  , nc_put_var1_vlen
  , nc_get_vlen_att
  , nc_get_scalar_vlen_att
  , nc_put_vlen_att
  , nc_put_scalar_vlen_att
) where

import           Control.Monad (void)
import           Data.Int
import qualified Data.Vector.Storable as VS
import           Foreign.Concurrent (addForeignPtrFinalizer)
import           Foreign.Storable
import           Foreign.ForeignPtr hiding (addForeignPtrFinalizer)
import           Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import           Foreign.Marshal.Utils (with)
import           GHC.TypeNats (Nat, KnownNat)

import           Internal.Definitions
import           Data.Format.NetCDF.LowLevel.Definitions
import           Data.Format.NetCDF.LowLevel.C.Definitions
import           Data.Format.NetCDF.LowLevel.Attribute
                  ( nc_put_scalar_att
                  , nc_get_att
                  , nc_put_att
                    )
import           Data.Format.NetCDF.LowLevel.Variable
                  ( nc_get_vara_fptr
                  , nc_get_var_fptr
                  , nc_get_vars_fptr
                  , nc_get_var1
                  , nc_get_scalar
                  , nc_put_scalar
                  , nc_put_var1)
import           Data.Format.NetCDF.LowLevel.User.Type


ncVLenForeignPtrToVS :: Int32 -> (ForeignPtr (NCVLenContainer 'U a), Int) -> IO (Int32, VS.Vector (NCVLenContainer 'M a))
ncVLenForeignPtrToVS res (fp, len) = if res /= 0
  then return (res, VS.empty)
  else do
    addForeignPtrFinalizer fp (void $ nc_free_vlens (fromIntegral len) (unsafeForeignPtrToPtr fp))
    return (res, VS.unsafeFromForeignPtr0 (castForeignPtr fp) len)

{-# INLINE fromScalarVLen #-}
fromScalarVLen :: forall a. Storable a => (Int32, NCVLenContainer 'U a) -> IO (Int32, VS.Vector a)
fromScalarVLen (res, ncVLenContainer) = if res /= 0
    then return (res, VS.empty)
    else do
      ncData <- peekVLenArray ncVLenContainer
      void $ with ncVLenContainer nc_free_vlen
      return (res, ncData)

nc_get_vara_vlen :: forall id a (t :: NCDataTypeTag) (n :: Nat).
  (KnownNat n, a ~ EquivalentHaskellType t, Storable a) =>
     NC id
  -> NCVariableId n ('TNCVLen t)
  -> StaticVector n Int
  -> StaticVector n Int
  -> IO (Int32, VS.Vector (NCVLenContainer 'M a))
nc_get_vara_vlen ncid varid start count = do
  (res, fpLen) <- nc_get_vara_fptr ncid varid start count
  ncVLenForeignPtrToVS res fpLen

nc_get_var1_vlen :: forall id a (t :: NCDataTypeTag) (n :: Nat).
  (KnownNat n, a ~ EquivalentHaskellType t, Storable a) =>
     NC id
  -> NCVariableId n ('TNCVLen t)
  -> StaticVector n Int
  -> IO (Int32, VS.Vector a)
nc_get_var1_vlen ncid varid start = do
  nc_get_var1 ncid varid start >>= fromScalarVLen

nc_get_var_vlen :: forall id a (t :: NCDataTypeTag) (n :: Nat).
  (KnownNat n, a ~ EquivalentHaskellType t, Storable a) =>
     NC id
  -> NCVariableId n ('TNCVLen t)
  -> IO (Int32, VS.Vector (NCVLenContainer 'M a))
nc_get_var_vlen ncid varid = do
    (res, fpLen) <- nc_get_var_fptr ncid varid
    ncVLenForeignPtrToVS res fpLen

nc_get_vars_vlen :: forall id a (t :: NCDataTypeTag) (n :: Nat).
  (KnownNat n, a ~ EquivalentHaskellType t, Storable a) =>
     NC id
  -> NCVariableId n ('TNCVLen t)
  -> StaticVector n Int
  -> StaticVector n Int
  -> StaticVector n Int
  -> IO (Int32, VS.Vector (NCVLenContainer 'M a))
nc_get_vars_vlen ncid varid start count stride = do
    (res, fpLen) <- nc_get_vars_fptr ncid varid start count stride
    ncVLenForeignPtrToVS res fpLen

nc_get_vlen :: forall id a (t :: NCDataTypeTag).
  (a ~ EquivalentHaskellType t, Storable a) =>
     NC id
  -> NCVariableId 0 ('TNCVLen t)
  -> IO (Int32, VS.Vector a)
nc_get_vlen ncid varid = do
  nc_get_scalar ncid varid >>= fromScalarVLen

nc_put_vlen :: forall id a (t :: NCDataTypeTag).
  (a ~ EquivalentHaskellType t, Storable a) =>
     NC id
  -> NCVariableId 0 ('TNCVLen t)
  -> VS.Vector a
  -> IO (Int32, ())
nc_put_vlen ncid varid ncData =
  withVLen ncData $ \ncDataPtr ->
    nc_put_scalar ncid varid ncDataPtr

nc_put_var1_vlen :: forall id a (t :: NCDataTypeTag) (n :: Nat).
  (KnownNat n, a ~ EquivalentHaskellType t, Storable a) =>
     NC id
  -> NCVariableId n ('TNCVLen t)
  -> StaticVector n Int
  -> VS.Vector a
  -> IO (Int32, ())
nc_put_var1_vlen ncid varid start ncData =
  withVLen ncData $ \ncDataPtr ->
    nc_put_var1 ncid varid start ncDataPtr

nc_get_vlen_att :: forall id a (t :: NCDataTypeTag).
  (a ~ EquivalentHaskellType t, Storable a) =>
     NC id
  -> NCAttribute ('TNCVLen t)
  -> IO (Int32, [VS.Vector a])
nc_get_vlen_att ncid attr = do
  (res, ncVLenContainer) <- nc_get_att ncid attr
  vlen <- if res /= 0
    then return []
    else reverse <$> VS.foldM' unwrap [] ncVLenContainer
  return (res, vlen)
  where
    unwrap res vlen =
      (:res) <$> peekVLenArray vlen <* with vlen nc_free_vlen

nc_get_scalar_vlen_att :: forall id a (t :: NCDataTypeTag).
  (a ~ EquivalentHaskellType t, Storable a) =>
     NC id
  -> NCAttribute ('TNCVLen t)
  -> IO (Int32, VS.Vector a)
nc_get_scalar_vlen_att ncid attr = do
  -- Instead of checking that attribute is indeed scalar, we simply
  -- read the whole thing and then return the first element of the
  -- retrieved Vector. However, in case of VLen attributes, all elements
  -- of the said Vector should be deallocated since memory allocation
  -- happens on the C-side for VLens.
  (res, ncVLenContainer) <- nc_get_att ncid attr
  vlen <- if res /= 0 || VS.null ncVLenContainer
    then return VS.empty
    else peekVLenArray (VS.head ncVLenContainer) <* freeVLenArray ncVLenContainer
  return (res, vlen)

nc_put_vlen_att :: forall id a (vt :: NCDataTypeTag) (at :: NCDataTypeTag) (n :: Nat).
  (a ~ EquivalentHaskellType at, Storable a) =>
     NC id
  -> Maybe (NCVariableId n vt)
  -> String
  -> NCType ('TNCVLen at)
  -> [VS.Vector a]
  -> IO (Int32, NCAttribute ('TNCVLen at))
nc_put_vlen_att ncid varid attrName attrType attrValue =
  withVLenList attrValue $ nc_put_att ncid varid attrName attrType

nc_put_scalar_vlen_att :: forall id a (vt :: NCDataTypeTag) (at :: NCDataTypeTag) (n :: Nat).
  (a ~ EquivalentHaskellType at, Storable a) =>
     NC id
  -> Maybe (NCVariableId n vt)
  -> String
  -> NCType ('TNCVLen at)
  -> VS.Vector a
  -> IO (Int32, NCAttribute ('TNCVLen at))
nc_put_scalar_vlen_att ncid varid attrName attrType attrValue =
  withVLen attrValue $ nc_put_scalar_att ncid varid attrName attrType
