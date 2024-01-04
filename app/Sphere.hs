module Sphere where

import Control.Monad (guard)
import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Records (HasField (getField))
import HitRecord (HitRecord, mkHitRecord)
import Hittable (Hittable (..))
import Interval2D (Interval2D, contains)
import Point (Point (..), scale)
import Ray (Ray (direction, origin), rayAt)
import UnitVector (UnitVector (UnitVector), dot, mkUnitVector)
import Vector (Vector (Vector), magnitudeSquared)
import Prelude

-- | A sphere.
data Sphere (a :: Type) where
  Sphere ::
    { -- | The center of the sphere.
      center :: Point a,
      -- | The radius of the sphere.
      radius :: a
    } ->
    Sphere a
  deriving (Eq, Ord, Show)

instance forall a. (Floating a, Ord a) => Hittable Sphere a where
  hit ::
    Sphere a ->
    Interval2D a ->
    Ray a ->
    Maybe (HitRecord a)
  hit sphere interval ray = do
    oc <- mkUnitVector $ coerce (ray.origin - sphere.center)
    let negHalfB :: a
        negHalfB = Prelude.negate (oc `UnitVector.dot` ray.direction)
        c :: a
        c = magnitudeSquared (coerce oc) - sphere.radius ^ (2 :: Int)
        discriminant :: a
        discriminant = negHalfB ^ (2 :: Int) - c
        sqrtd :: a
        sqrtd = sqrt discriminant
        root1 :: a
        root1 = negHalfB - sqrtd
        root2 :: a
        root2 = negHalfB + sqrtd
    guard (discriminant >= 0)
    root <- case interval `contains` root1 of
      True -> pure root1
      False -> case interval `contains` root2 of
        True -> pure root2
        False -> Nothing
    let point :: Point a
        point = rayAt ray root
    unitVector <- mkUnitVector $ coerce (recip sphere.radius `Point.scale` (point - sphere.center))
    pure $ mkHitRecord point unitVector root ray
