{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Format.HDF.LowLevel.Definitions where

import           Data.Int
import           Data.Kind
import           Data.Type.Equality (TestEquality, testEquality, type(==), (:~:)(Refl))
import qualified Data.Vector.Storable as VS
import           Data.Word
import           Foreign.Ptr (castPtr)
import           Foreign.Storable (Storable(..))
import           GHC.TypeLits (TypeError, ErrorMessage(..))

data HDFData

newtype Char8 = Char8 Int8 deriving (Eq, Show)
newtype UChar8 = UChar8 Word8 deriving (Eq, Show)

instance Storable Char8 where
    sizeOf    (Char8  c) = sizeOf    c
    alignment (Char8  c) = alignment c
    peek cPtr            = Char8 <$> peek (castPtr cPtr)
    poke cPtr (Char8  c) = poke (castPtr cPtr) c

instance Storable UChar8 where
    sizeOf    (UChar8 c) = sizeOf    c
    alignment (UChar8 c) = alignment c
    peek cPtr            = UChar8 <$> peek (castPtr cPtr)
    poke cPtr (UChar8 c) = poke (castPtr cPtr) c

data HDataType a where
    HNone :: HDataType ()

    HUChar8 :: HDataType UChar8
    HChar8 :: HDataType Char8

    HWord8 :: HDataType Word8
    HWord16 :: HDataType Word16
    HWord32 :: HDataType Word32
    HInt8 :: HDataType Int8
    HInt16 :: HDataType Int16
    HInt32 :: HDataType Int32
    HFloat32 :: HDataType Float
    HFloat64 :: HDataType Double

deriving instance Show (HDataType a)

instance TestEquality HDataType where
    testEquality a b = case a of
      HNone -> case b of
        HNone -> Just Refl
        _ -> Nothing
      HUChar8 -> case b of
        HUChar8 -> Just Refl
        _ -> Nothing
      HChar8 -> case b of
        HChar8 -> Just Refl
        _ -> Nothing
      HWord8 -> case b of
        HWord8 -> Just Refl
        _ -> Nothing
      HWord16 -> case b of
        HWord16 -> Just Refl
        _ -> Nothing
      HWord32 -> case b of
        HWord32 -> Just Refl
        _ -> Nothing
      HInt8 -> case b of
        HInt8 -> Just Refl
        _ -> Nothing
      HInt16 -> case b of
        HInt16 -> Just Refl
        _ -> Nothing
      HInt32 -> case b of
        HInt32 -> Just Refl
        _ -> Nothing
      HFloat32 -> case b of
        HFloat32 -> Just Refl
        _ -> Nothing
      HFloat64 -> case b of
        HFloat64 -> Just Refl
        _ -> Nothing

data HKind where
    Empty :: HKind
    Nullary :: HKind
    Unary :: (Type -> Type) -> HKind

data HDFValue (a :: HKind) where
    HDFValue :: (Show (SelectKind a t), Eq (SelectKind a t), Storable t, Show t, Eq t) =>
        {hValueType :: HDataType t, hValue :: SelectKind a t} -> HDFValue a

deriving instance Show (HDFValue a)

type HDFType = HDFValue 'Empty
type HDFVector = HDFValue ('Unary VS.Vector)

instance Eq (HDFValue a) where
  (HDFValue t1 a) == (HDFValue t2 b) = case testEquality t1 t2 of
    Just Refl -> a == b
    Nothing -> False

type family SelectKind (a :: HKind) (t :: Type) :: Type where
    SelectKind 'Empty _ = ()
    SelectKind 'Nullary t = t
    SelectKind ('Unary v) t = v t

type family OneOf (a :: k) (xs :: [k]) :: Constraint where
  OneOf a xs = (OneOfInternal 'False a xs xs) ~ 'True

type family OneOfInternal (found :: Bool) (a :: k) (xs :: [k]) (all :: [k]) :: Bool where
  OneOfInternal 'True  _  _        _   = 'True
  OneOfInternal 'False a '[]       all = TypeError ('ShowType a ':<>: 'Text " is not found in " ':<>: 'ShowType all)
  OneOfInternal  res   a (b ': xs) all = OneOfInternal (a == b) a xs all
