{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Internal.Numerals.Ternary(
    Ternary
  , toTernary
  , fromTernary
  , TernarySNat(..)
  , SomeTernarySNat(..)
  , toTernarySNat
  , fromTernarySNat
  , snat3
) where

import Data.Type.Bool
import Data.Type.Ord
import GHC.TypeNats (Nat, Natural, Mod, Div, type(+), type(*))

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

data Ternary where
  TBot :: Ternary
  T0 :: Ternary -> Ternary
  T1 :: Ternary -> Ternary
  T2 :: Ternary -> Ternary deriving Show

type family TTernary (t :: Ternary) (n :: Nat) :: Ternary where
  TTernary t 0 = t
  TTernary t n = If (OrdCond (Compare (n `Mod` 3) 2) 'False 'True 'False)
    (TTernary ('T2 t) (n `Div` 3))
    (If (OrdCond (Compare (n `Mod` 3) 1) 'False 'True 'False)
       (TTernary ('T1 t) (n `Div` 3))
       (TTernary ('T0 t) (n `Div` 3)))

type family FTernary (t :: Ternary) (n :: Nat) (b :: Nat) :: Nat where
  FTernary TBot r _ = r
  FTernary (T0 rest) r b = FTernary rest r         (b*3)
  FTernary (T1 rest) r b = FTernary rest (r +   b) (b*3)
  FTernary (T2 rest) r b = FTernary rest (r + 2*b) (b*3)

type TReverse (x :: Ternary) = TReverseQ 'TBot x

type family TReverseQ (x :: Ternary) (y :: Ternary) :: Ternary where
    TReverseQ x 'TBot = x
    TReverseQ x ('T1 y) = TReverseQ ('T1 x) y
    TReverseQ x ('T0 y) = TReverseQ ('T0 x) y
    TReverseQ x ('T2 y) = TReverseQ ('T2 x) y

reverseT :: Ternary -> Ternary
reverseT = go TBot
  where
   go :: Ternary -> Ternary -> Ternary
   go x TBot = x
   go x (T1 rest) = go (T1 x) rest
   go x (T2 rest) = go (T2 x) rest
   go x (T0 rest) = go (T0 x) rest

-- TernarySNat should be constructed in reversed order to correctly
-- track the represented number in its type-level parameter.
data TernarySNat (n :: Nat) where
  STBot :: TernarySNat 0
  ST0 :: TernarySNat n -> TernarySNat (FTernary ('T0 (TReverse (TTernary 'TBot n))) 0 1)
  ST1 :: TernarySNat n -> TernarySNat (FTernary ('T1 (TReverse (TTernary 'TBot n))) 0 1)
  ST2 :: TernarySNat n -> TernarySNat (FTernary ('T2 (TReverse (TTernary 'TBot n))) 0 1)

deriving instance Show (TernarySNat n)

data SomeTernarySNat where
  SomeTernarySNat :: forall n. {unwrapTernarySNat :: TernarySNat n} -> SomeTernarySNat

deriving instance Show (SomeTernarySNat)

toTernarySNat :: Natural -> SomeTernarySNat
toTernarySNat = t2st . toTernary

t2st :: Ternary -> SomeTernarySNat
t2st = go (SomeTernarySNat STBot)
  where
    go :: SomeTernarySNat -> Ternary -> SomeTernarySNat
    go r@(SomeTernarySNat res) s = case s of
      TBot    -> r
      T0 rest -> go (SomeTernarySNat $! ST0 res) rest
      T1 rest -> go (SomeTernarySNat $! ST1 res) rest
      T2 rest -> go (SomeTernarySNat $! ST2 res) rest

fromTernarySNat :: TernarySNat n -> Natural
fromTernarySNat n = go 0 1 n
  where
    go :: Natural -> Natural -> TernarySNat n -> Natural
    go res base s = case s of
      STBot -> res
      ST0 rest -> go res (base*3) rest
      ST1 rest -> go (res +   base) (base*3) rest
      ST2 rest -> go (res + 2*base) (base*3) rest

toTernary :: Natural -> Ternary
toTernary = go TBot
  where
  go :: Ternary -> Natural -> Ternary
  go TBot 0 = T0 TBot
  go res  0 = res
  go res num
      | num `mod` 3 ==  2 = go (T2 res) $! num `div` 3
      | num `mod` 3 ==  1 = go (T1 res) $! num `div` 3
      | otherwise         = go (T0 res) $! num `div` 3

fromTernary :: Ternary -> Natural
fromTernary n = go 0 1 n
  where
    go :: Natural -> Natural -> Ternary -> Natural
    go res base s = case s of
      TBot -> res
      T0 rest -> go res (base*3) rest
      T1 rest -> go (res +   base) (base*3) rest
      T2 rest -> go (res + 2*base) (base*3) rest

snat3 :: QuasiQuoter
snat3 = QuasiQuoter {
    quoteDec  = error errorMessage
  , quoteExp  = convertToTernaryE
  , quotePat  = convertToTernaryP
  , quoteType = error errorMessage}
  where
    errorMessage :: String
    errorMessage = "ternary QuasiQuoter can be used only within pattern or expression context"

convertToTernaryE :: String -> Q Exp
convertToTernaryE str = do
  let converted = buildTernaryTHE (ConE (mkName "STBot")) . toTernary . read $ str
  return converted

buildTernaryTHE :: Exp -> Ternary -> Exp
buildTernaryTHE res TBot = res
buildTernaryTHE res (T2 rest) = buildTernaryTHE (AppE (ConE (mkName "ST2")) res) rest
buildTernaryTHE res (T1 rest) = buildTernaryTHE (AppE (ConE (mkName "ST1")) res) rest
buildTernaryTHE res (T0 rest) = buildTernaryTHE (AppE (ConE (mkName "ST0")) res) rest


convertToTernaryP :: String -> Q Pat
convertToTernaryP str = do
  let converted = buildTernaryTHP (ConP (mkName "STBot") [] []) . toTernary . read $ str
  return converted

buildTernaryTHP :: Pat -> Ternary -> Pat
buildTernaryTHP res TBot = res
buildTernaryTHP res (T2 rest) = buildTernaryTHP (ConP (mkName "ST2") [] [res]) rest
buildTernaryTHP res (T1 rest) = buildTernaryTHP (ConP (mkName "ST1") [] [res]) rest
buildTernaryTHP res (T0 rest) = buildTernaryTHP (ConP (mkName "ST0") [] [res]) rest
