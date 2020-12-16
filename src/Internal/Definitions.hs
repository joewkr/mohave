{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Internal.Definitions where

import qualified Data.ByteString as BS
import           Data.Int
import           Data.Function (on)
import           Data.Kind
import           Data.Proxy (Proxy(..))
import           Data.Type.Equality (TestEquality, testEquality, (:~:)(Refl))
import qualified Data.Vector.Storable as VS
import           Data.Word
import           Foreign.C.Types (CChar)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable(..))
import           Foreign.Marshal.Array (allocaArray, advancePtr)
import           Foreign.Marshal.Utils (with)
import           GHC.TypeNats
import           Unsafe.Coerce


type family PtrLoad (a :: Type) :: Type where
    PtrLoad BS.ByteString = CChar
    PtrLoad t = t

class Marshallable a where
    withPtr :: a -> (Ptr (PtrLoad a) -> IO b) -> IO b
    fromPtr :: Ptr (PtrLoad a) -> IO a

instance Marshallable ()     where
    withPtr = with
    fromPtr = peek

instance Marshallable Int8   where
    withPtr = with
    fromPtr = peek

instance Marshallable Word8  where
    withPtr = with
    fromPtr = peek

instance Marshallable Int16  where
    withPtr = with
    fromPtr = peek

instance Marshallable Word16 where
    withPtr = with
    fromPtr = peek

instance Marshallable Int32  where
    withPtr = with
    fromPtr = peek

instance Marshallable Word32 where
    withPtr = with
    fromPtr = peek

instance Marshallable Int64  where
    withPtr = with
    fromPtr = peek

instance Marshallable Word64 where
    withPtr = with
    fromPtr = peek

instance Marshallable Float  where
    withPtr = with
    fromPtr = peek

instance Marshallable Double where
    withPtr = with
    fromPtr = peek

instance Marshallable BS.ByteString where
    withPtr = BS.useAsCString
    fromPtr = BS.packCString

data VarShapeDef (n :: Nat) a where
    ScalarVar :: VarShapeDef 0 a
    ArrayVar  :: StaticVector n a -> VarShapeDef n a

data StaticVector (n :: Nat) a where
    D :: a -> StaticVector 1 a
    (:|) :: StaticVector n a -> a -> StaticVector (n + 1) a

deriving instance Show a => Show (StaticVector n a)

instance Eq a => Eq (StaticVector n a) where
    (==) = (==) `on` fromStaticVector

instance Functor (StaticVector n) where
    fmap f (D a)       = D $! f a
    fmap f (rest :| a) = (fmap f rest) :| (f a)

withStaticVector :: forall a (n :: Nat) b. (KnownNat n, Storable a) =>
    StaticVector n a -> (Ptr a -> IO b) -> IO b
withStaticVector index f = allocaArray indexRank $ \indexPtr -> do
    fillIndexArray (advancePtr indexPtr (indexRank - 1)) index
    res <- f indexPtr
    return $! res
  where
    indexRank :: Int
    indexRank = fromIntegral $ natVal (Proxy :: Proxy n)

fillIndexArray :: Storable a => Ptr a -> StaticVector n a -> IO ()
fillIndexArray ptr idx = case idx of
    (D a) -> poke ptr a
    (:|) rest a -> do
        poke ptr a
        fillIndexArray (advancePtr ptr (-1)) rest

data W a where
    W :: forall a (n :: Nat). StaticVector n a -> W a

fromStaticVector :: forall a (n :: Nat). StaticVector n a -> [a]
fromStaticVector = reverse . go
  where
    go :: forall b (m :: Nat). StaticVector m b -> [b]
    go (D     v) = [v]
    go (vs :| v) = v:(go vs)

toStaticVector :: forall a (n :: Nat). KnownNat n =>
    Proxy n -> [a] -> Maybe (StaticVector n a)
toStaticVector _ [] = Nothing
toStaticVector p l@(v:vs) =
    if (natVal p) > 0 && (natVal p) == fromIntegral (length l)
        then convert p w
        else Nothing
  where
    w = foldl (\(W x) n -> W $ x :| n) (W $ D v) vs

    convert _ (W (vec :: StaticVector m a)) =
        case forceEquality (Proxy :: Proxy n) (Proxy :: Proxy m) of
            Refl -> Just vec

-- In GHC Nat is a non-inductive type, and KnownNat (n + 1) could
-- not be deduced from KnownNat n, which makes converting a list to
-- StaticVector of required size quite a non-trivial exercise. This
-- could be achieved by using ghc-typelits-knownnat type checker plugin,
-- but using it would introduce a new non-trivial dependency. So,
-- taking into account that requested StaticVector size and provided
-- vector length are known at runtime and could be checked for equality,
-- we just use a dirty unsafeCoerce hack to stop GHC from complaining.
forceEquality :: forall n m. Proxy n -> Proxy m -> n :~: m
forceEquality _ _ = unsafeCoerce (Refl :: n :~: n)

data ValueKind where
    Empty :: ValueKind
    Nullary :: ValueKind
    Unary :: (Type -> Type) -> ValueKind

data TypedValue (a :: ValueKind) dt where
    TypedValue :: (
        Show (SelectKind a t)
      , Eq (SelectKind a t)
      , Marshallable t
      , Show (dt t)
      , TestEquality dt
      , Eq t) =>
        {valueType :: dt t, value :: SelectKind a t} -> TypedValue a dt

deriving instance Show (TypedValue a dt)

type TType = TypedValue 'Empty
type TScalar = TypedValue 'Nullary
type TVector = TypedValue ('Unary VS.Vector)

instance Eq (TypedValue a dt) where
  (TypedValue t1 a) == (TypedValue t2 b) = case testEquality t1 t2 of
    Just Refl -> a == b
    Nothing -> False

type family SelectKind (a :: ValueKind) (t :: Type) :: Type where
    SelectKind 'Empty _ = ()
    SelectKind 'Nullary t = t
    SelectKind ('Unary v) t = v t
