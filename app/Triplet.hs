module Triplet where

import Data.Kind (Type)
import Prelude

newtype Triplet (a :: Type) where
  Triplet :: (a, a, a) -> Triplet a
  deriving newtype
    ( Eq,
      Ord,
      Show
    )

instance Functor Triplet where
  fmap :: (a -> b) -> Triplet a -> Triplet b
  fmap f (Triplet (a, b, c)) = Triplet (f a, f b, f c)

instance Applicative Triplet where
  pure :: a -> Triplet a
  pure a = Triplet (a, a, a)
  (<*>) :: Triplet (a -> b) -> Triplet a -> Triplet b
  (<*>) (Triplet (f, g, h)) (Triplet (a, b, c)) = Triplet (f a, g b, h c)
  liftA2 :: (a -> b -> c) -> Triplet a -> Triplet b -> Triplet c
  liftA2 f (Triplet (a, b, c)) (Triplet (x, y, z)) = Triplet (f a x, f b y, f c z)

instance Foldable Triplet where
  foldr :: (a -> b -> b) -> b -> Triplet a -> b
  foldr f z (Triplet (a, b, c)) = f a (f b (f c z))

instance (Num a) => Num (Triplet a) where
  (+) :: Triplet a -> Triplet a -> Triplet a
  (+) = liftA2 (+)
  (-) :: Triplet a -> Triplet a -> Triplet a
  (-) = liftA2 (-)
  (*) :: Triplet a -> Triplet a -> Triplet a
  (*) = liftA2 (*)
  negate :: Triplet a -> Triplet a
  negate = fmap Prelude.negate
  abs :: Triplet a -> Triplet a
  abs = fmap abs
  signum :: Triplet a -> Triplet a
  signum = fmap signum
  fromInteger :: Integer -> Triplet a
  fromInteger = pure . fromInteger

instance (Fractional a) => Fractional (Triplet a) where
  (/) :: Triplet a -> Triplet a -> Triplet a
  (/) = liftA2 (/)
  recip :: Triplet a -> Triplet a
  recip = fmap recip
  fromRational :: Rational -> Triplet a
  fromRational = pure . fromRational

instance (Floating a) => Floating (Triplet a) where
  pi :: Triplet a
  pi = pure pi
  exp :: Triplet a -> Triplet a
  exp = fmap exp
  log :: Triplet a -> Triplet a
  log = fmap log
  sin :: Triplet a -> Triplet a
  sin = fmap sin
  cos :: Triplet a -> Triplet a
  cos = fmap cos
  asin :: Triplet a -> Triplet a
  asin = fmap asin
  acos :: Triplet a -> Triplet a
  acos = fmap acos
  atan :: Triplet a -> Triplet a
  atan = fmap atan
  sinh :: Triplet a -> Triplet a
  sinh = fmap sinh
  cosh :: Triplet a -> Triplet a
  cosh = fmap cosh
  asinh :: Triplet a -> Triplet a
  asinh = fmap asinh
  acosh :: Triplet a -> Triplet a
  acosh = fmap acosh
  atanh :: Triplet a -> Triplet a
  atanh = fmap atanh
