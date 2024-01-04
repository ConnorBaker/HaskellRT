module UnitVector where

import Control.Monad (guard)
import Data.Coerce (coerce)
import Data.Function (applyWhen)
import Data.Kind (Type)
import Data.Random (Distribution, RVar, StdUniform)
import Triplet (Triplet (Triplet))
import Vector
  ( Vector (..),
    dot,
    magnitudeSquared,
    sampleVectorStdUniform,
    scale,
  )
import Prelude

newtype UnitVector (a :: Type) where
  UnitVector :: Vector a -> UnitVector a
  deriving newtype (Eq, Ord, Show, Foldable)

negate :: (Num a) => UnitVector a -> UnitVector a
negate = UnitVector . Prelude.negate . coerce

dot :: (Num a) => UnitVector a -> UnitVector a -> a
dot u1 u2 = Vector.dot (coerce u1) (coerce u2)

mkUnitVector :: forall a. (Floating a, Eq a) => Vector a -> Maybe (UnitVector a)
mkUnitVector v = do
  let magSq :: a
      magSq = magnitudeSquared v
      normalizedVector :: Vector a
      normalizedVector = recip (sqrt magSq) `scale` v
  guard (magSq /= 0)
  pure (coerce normalizedVector)

-- | Generates a random unit vector in 3D space.
randomUniformUnitVector :: (Ord a, Floating a, Distribution StdUniform a) => RVar (UnitVector a)
randomUniformUnitVector = do
  v <- sampleVectorStdUniform
  maybe randomUniformUnitVector pure (mkUnitVector v)

-- | Generates a random unit vector in the hemisphere of the given normal.
randomUniformUnitVectorOnUnitHemisphere ::
  forall a.
  (Ord a, Floating a, Distribution StdUniform a) =>
  -- | The normal of the hemisphere.
  UnitVector a ->
  -- | The randomly generated unit vector.
  RVar (UnitVector a)
randomUniformUnitVectorOnUnitHemisphere normal = do
  u <- randomUniformUnitVector
  let dotProduct :: a
      dotProduct = UnitVector.dot u normal
  pure $ applyWhen (dotProduct < 0) UnitVector.negate u
