module Vector where

import Control.Monad (join)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Random (Distribution, RVar, StdUniform, stdUniform)
import Triplet (Triplet (..))
import Prelude

newtype Vector (a :: Type) where
  Vector :: Triplet a -> Vector a
  deriving newtype (Eq, Ord, Show, Num, Fractional, Floating, Functor, Applicative, Foldable)

cross :: (Num a) => Vector a -> Vector a -> Vector a
cross v1 v2 =
  let (Vector (Triplet (a1, a2, a3))) = v1
      (Vector (Triplet (b1, b2, b3))) = v2
   in coerce (a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1)

shift :: (Num a) => a -> Vector a -> Vector a
shift = (+) . pure

scale :: (Num a) => a -> Vector a -> Vector a
scale = (*) . pure

dot :: (Num a) => Vector a -> Vector a -> a
dot = (sum .) . (*)

magnitudeSquared :: (Num a) => Vector a -> a
magnitudeSquared = sum . fmap (^ (2 :: Int))

magnitude :: (Floating a) => Vector a -> a
magnitude = sqrt . magnitudeSquared

sampleVectorStdUniform :: (Ord a, Floating a, Distribution StdUniform a) => RVar (Vector a)
sampleVectorStdUniform = do
  x <- stdUniform
  y <- stdUniform
  z <- stdUniform
  pure . Vector $ Triplet (x, y, z)

sampleVectorStdUniformWhile :: (Ord a, Floating a, Distribution StdUniform a) => (Vector a -> Bool) -> RVar (Vector a)
sampleVectorStdUniformWhile p = do
  v <- sampleVectorStdUniform
  case p v of
    True -> pure v
    False -> sampleVectorStdUniformWhile p
