{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- QuickCheck-like property tests for SIMD vector operations.

module Main (main) where

import Data.Coerce
import Data.Word
import GHC.Prim
import GHC.Exts
import GHC.Float
  ( castFloatToWord32 , castWord32ToFloat
  , castDoubleToWord64, castWord64ToDouble
  )

import MiniQuickCheck

--------------------------------------------------------------------------------
-- Scalar wrappers that use bit-equality to test for equality.

newtype FloatNT = FloatNT Float
  deriving newtype (Show, Num)

instance Eq FloatNT where
  FloatNT f1 == FloatNT f2 = castFloatToWord32 f1 == castFloatToWord32 f2

instance Arbitrary FloatNT where
  arbitrary = FloatNT . castWord32ToFloat <$> arbitrary

newtype DoubleNT = DoubleNT Double
  deriving newtype (Show, Num)

instance Eq DoubleNT where
  DoubleNT d1 == DoubleNT d2 = castDoubleToWord64 d1 == castDoubleToWord64 d2

instance Arbitrary DoubleNT where
  arbitrary = DoubleNT . castWord64ToDouble <$> arbitrary

--------------------------------------------------------------------------------
-- Min/max for the types under test

class HasMinMax a where
  mini, maxi :: a -> a -> a

instance HasMinMax FloatNT where
  mini (FloatNT (F# f1)) (FloatNT (F# f2)) = FloatNT (F# (minFloat# f1 f2))
  maxi (FloatNT (F# f1)) (FloatNT (F# f2)) = FloatNT (F# (maxFloat# f1 f2))

instance HasMinMax DoubleNT where
  mini (DoubleNT (D# d1)) (DoubleNT (D# d2)) = DoubleNT (D# (minDouble# d1 d2))
  maxi (DoubleNT (D# d1)) (DoubleNT (D# d2)) = DoubleNT (D# (maxDouble# d1 d2))

--------------------------------------------------------------------------------
-- SIMD vector types

data FloatX4 = FX4# FloatX4#

instance Show FloatX4 where
  show (FX4# f) = case unpackFloatX4# f of
    (# a, b, c, d #) -> show (F# a, F# b, F# c, F# d)

instance Eq FloatX4 where
  FX4# a == FX4# b
    = case unpackFloatX4# a of
        (# a1, a2, a3, a4 #) ->
          case unpackFloatX4# b of
            (# b1, b2, b3, b4 #) ->
              FloatNT (F# a1) == FloatNT (F# b1) &&
              FloatNT (F# a2) == FloatNT (F# b2) &&
              FloatNT (F# a3) == FloatNT (F# b3) &&
              FloatNT (F# a4) == FloatNT (F# b4)

instance Arbitrary FloatX4 where
  arbitrary = do
    FloatNT (F# f1) <- arbitrary
    FloatNT (F# f2) <- arbitrary
    FloatNT (F# f3) <- arbitrary
    FloatNT (F# f4) <- arbitrary
    return $ FX4# (packFloatX4# (# f1, f2, f3, f4 #))

instance Num FloatX4 where
  FX4# x + FX4# y = FX4# (x `plusFloatX4#`  y)
  FX4# x - FX4# y = FX4# (x `minusFloatX4#` y)
  negate (FX4# x) = FX4# (negateFloatX4# x)
  FX4# x * FX4# y = FX4# (x `timesFloatX4#` y)
  abs    = error "FloatX4: no abs"
  signum = error "FloatX4: no signum"
  fromInteger = error "FloatX4: no fromInteger"

instance HasMinMax FloatX4 where
  mini (FX4# a) (FX4# b) = FX4# (minFloatX4# a b)
  maxi (FX4# a) (FX4# b) = FX4# (maxFloatX4# a b)

--------------------------------------------------------------------------------

data DoubleX2 = DX2# DoubleX2#

instance Show DoubleX2 where
  show (DX2# d) = case unpackDoubleX2# d of
    (# a, b #) -> show (D# a, D# b)

instance Eq DoubleX2 where
  DX2# a == DX2# b
    = case unpackDoubleX2# a of
        (# a1, a2 #) ->
          case unpackDoubleX2# b of
            (# b1, b2 #) ->
              DoubleNT (D# a1) == DoubleNT (D# b1) &&
              DoubleNT (D# a2) == DoubleNT (D# b2)

instance Arbitrary DoubleX2 where
  arbitrary = do
    DoubleNT (D# d1) <- arbitrary
    DoubleNT (D# d2) <- arbitrary
    return $ DX2# (packDoubleX2# (# d1, d2 #))

instance Num DoubleX2 where
  DX2# x + DX2# y = DX2# (x `plusDoubleX2#`  y)
  DX2# x - DX2# y = DX2# (x `minusDoubleX2#` y)
  negate (DX2# x) = DX2# (negateDoubleX2# x)
  DX2# x * DX2# y = DX2# (x `timesDoubleX2#` y)
  abs    = error "DoubleX2: no abs"
  signum = error "DoubleX2: no signum"
  fromInteger = error "DoubleX2: no fromInteger"

instance HasMinMax DoubleX2 where
  mini (DX2# a) (DX2# b) = DX2# (minDoubleX2# a b)
  maxi (DX2# a) (DX2# b) = DX2# (maxDoubleX2# a b)

--------------------------------------------------------------------------------
-- Expression language for generating random expressions over vector types.

data Expr a where
  Lit :: a -> Expr a
  Add :: Expr a -> Expr a -> Expr a
  Sub :: Expr a -> Expr a -> Expr a
  Neg :: Expr a -> Expr a
  Mul :: Expr a -> Expr a -> Expr a
  Min :: Expr a -> Expr a -> Expr a
  Max :: Expr a -> Expr a -> Expr a
  deriving (Show, Eq)

fmapExpr :: (a -> b) -> Expr a -> Expr b
fmapExpr f (Lit a)   = Lit (f a)
fmapExpr f (Add a b) = Add (fmapExpr f a) (fmapExpr f b)
fmapExpr f (Sub a b) = Sub (fmapExpr f a) (fmapExpr f b)
fmapExpr f (Neg a)   = Neg (fmapExpr f a)
fmapExpr f (Mul a b) = Mul (fmapExpr f a) (fmapExpr f b)
fmapExpr f (Min a b) = Min (fmapExpr f a) (fmapExpr f b)
fmapExpr f (Max a b) = Max (fmapExpr f a) (fmapExpr f b)

instance Arbitrary a => Arbitrary (Expr a) where
  arbitrary = do
    (tag :: Word64) <- arbitrary
    case tag `mod` 20 of
      1 -> Add <$> arbitrary <*> arbitrary
      2 -> Sub <$> arbitrary <*> arbitrary
      3 -> Neg <$> arbitrary
      4 -> Mul <$> arbitrary <*> arbitrary
      5 -> Min <$> arbitrary <*> arbitrary
      6 -> Max <$> arbitrary <*> arbitrary
      _ -> Lit <$> arbitrary

eval :: (Num a, HasMinMax a) => Expr a -> a
eval (Lit a)   = a
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Neg a)   = negate (eval a)
eval (Mul a b) = eval a * eval b
eval (Min a b) = mini (eval a) (eval b)
eval (Max a b) = maxi (eval a) (eval b)

--------------------------------------------------------------------------------
-- Test groups

testFloatX4 :: Test
testFloatX4 = Group "FloatX4"
  [ Property "FloatX4" $ \ ( expr :: Expr FloatX4 ) ->
      unpack ( eval expr ) ===
        ( eval ( fmapExpr get1 expr )
        , eval ( fmapExpr get2 expr )
        , eval ( fmapExpr get3 expr )
        , eval ( fmapExpr get4 expr )
        )
  ]
  where
    unpack :: FloatX4 -> ( FloatNT, FloatNT, FloatNT, FloatNT )
    unpack (FX4# f) = case unpackFloatX4# f of
      (# f1, f2, f3, f4 #) -> coerce ( F# f1, F# f2, F# f3, F# f4 )

    get1, get2, get3, get4 :: FloatX4 -> FloatNT
    get1 (FX4# f) = case unpackFloatX4# f of (# f1,  _,  _,  _ #) -> FloatNT (F# f1)
    get2 (FX4# f) = case unpackFloatX4# f of (#  _, f2,  _,  _ #) -> FloatNT (F# f2)
    get3 (FX4# f) = case unpackFloatX4# f of (#  _,  _, f3,  _ #) -> FloatNT (F# f3)
    get4 (FX4# f) = case unpackFloatX4# f of (#  _,  _,  _, f4 #) -> FloatNT (F# f4)

testDoubleX2 :: Test
testDoubleX2 = Group "DoubleX2"
  [ Property "DoubleX2" $ \ ( expr :: Expr DoubleX2 ) ->
      unpack ( eval expr ) ===
        ( eval ( fmapExpr get1 expr )
        , eval ( fmapExpr get2 expr )
        )
  ]
  where
    unpack :: DoubleX2 -> ( DoubleNT, DoubleNT )
    unpack (DX2# d) = case unpackDoubleX2# d of
      (# d1, d2 #) -> coerce ( D# d1, D# d2 )

    get1, get2 :: DoubleX2 -> DoubleNT
    get1 (DX2# d) = case unpackDoubleX2# d of
      (# d1,  _ #) -> DoubleNT (D# d1)
    get2 (DX2# d) = case unpackDoubleX2# d of
      (#  _, d2 #) -> DoubleNT (D# d2)

testSIMD :: Test
testSIMD = Group "ALL" [testFloatX4, testDoubleX2]

main :: IO ()
main = runTestsMain (Iterations 100) testSIMD
