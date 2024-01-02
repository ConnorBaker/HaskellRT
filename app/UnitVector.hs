module UnitVector where

import Control.Monad (guard)
import Data.Coerce
import Data.Kind (Type)
import Triplet (Triplet (Triplet))
import Vector (Vector (..), dot, magnitudeSquared, scale)
import Prelude

newtype UnitVector (a :: Type) where
  UnitVector :: Vector a -> UnitVector a
  deriving newtype (Eq, Ord, Show, Foldable)

negate :: (Num a) => UnitVector a -> UnitVector a
negate = UnitVector . Prelude.negate . coerce

dot :: (Num a) => UnitVector a -> UnitVector a -> a
dot u1 u2 = Vector.dot (coerce u1) (coerce u2)

mkUnitVector :: (Floating a, Eq a) => Vector a -> Maybe (UnitVector a)
mkUnitVector v = do
  let magSq = magnitudeSquared v
      normalizedVector = recip (sqrt magSq) `scale` v
  guard (magSq /= 0)
  pure (coerce normalizedVector)
