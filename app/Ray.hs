module Ray where

import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Records (HasField (getField))
import Point (Point (..))
import Triplet (Triplet (..))
import UnitVector (UnitVector (..))
import Vector (Vector (Vector))
import Prelude

-- | A ray.
data Ray (a :: Type) where
  Ray ::
    { -- | The origin of the ray.
      origin :: Point a,
      -- | The direction of the ray.
      direction :: UnitVector a
    } ->
    Ray a
  deriving (Eq, Ord, Show)

-- | Get the point on the ray at time t.
rayAt ::
  (Floating a) =>
  -- | The ray.
  Ray a ->
  -- | The time.
  a ->
  -- | The point on the ray at time t.
  Point a
rayAt ray t = ray.origin + (pure t) * coerce ray.direction
