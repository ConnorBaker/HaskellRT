module HitRecord (HitRecord (..), mkHitRecord) where

import Data.Function (applyWhen)
import Data.Kind (Type)
import GHC.Records (HasField (getField))
import Point (Point)
import Ray (Ray (direction))
import UnitVector (UnitVector (..), dot, negate)
import Prelude

data HitRecord (a :: Type) where
  HitRecord ::
    { -- | The point of intersection.
      point :: Point a,
      -- | The normal vector at the point of intersection.
      normal :: UnitVector a,
      -- | The parameter value of the ray at the point of intersection.
      t :: a,
      -- | True if the ray is hitting the front face of the object.
      frontFace :: Bool
    } ->
    HitRecord a
  deriving (Eq, Show)

-- | Sets the normal of the hit record to the outward normal of the
-- surface hit by the ray. The outward normal is the normal that
-- points in the same direction as the ray.
mkHitRecord ::
  forall a.
  (Num a, Ord a) =>
  -- | The point of intersection.
  Point a ->
  -- | The outward normal vector at the point of intersection.
  UnitVector a ->
  -- | The parameter value of the ray at the point of intersection.
  a ->
  -- | True if the ray is hitting the front face of the object.
  Ray a ->
  -- | The 'HitRecord'.
  HitRecord a
mkHitRecord point outwardNormal t ray =
  let frontFace :: Bool
      frontFace = (ray.direction `dot` outwardNormal) < 0
      normal :: UnitVector a
      normal = applyWhen frontFace UnitVector.negate outwardNormal
   in HitRecord {..}
