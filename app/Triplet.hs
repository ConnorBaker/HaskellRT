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
  fmap f (Triplet (a, b, c)) = Triplet (f a, f b, f c)

instance Applicative Triplet where
  pure a = Triplet (a, a, a)
  (<*>) (Triplet (f, g, h)) (Triplet (a, b, c)) = Triplet (f a, g b, h c)
  liftA2 f (Triplet (a, b, c)) (Triplet (x, y, z)) = Triplet (f a x, f b y, f c z)

instance Foldable Triplet where
  foldr f z (Triplet (a, b, c)) = f a (f b (f c z))

instance (Num a) => Num (Triplet a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap Prelude.negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Fractional a) => Fractional (Triplet a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance (Floating a) => Floating (Triplet a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh
