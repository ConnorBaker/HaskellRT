module World where

import Data.Kind (Type)
import GHC.Records (HasField (getField))
import HitRecord (HitRecord (t))
import Interval2D (Interval2D (..))
import Point ()
import Ray (Ray)
import Sphere (Sphere, hit)
import Triplet ()
import UnitVector ()
import Prelude

-- | A world containing a list of objects.
newtype World (a :: Type) where
  World :: a -> World a
  deriving newtype
    ( Eq,
      Ord,
      Show
    )

-- TODO: We're expressing hitting an object in the world as this sequential operation where
-- we progressively update the interval.
-- Is there a way to express this without requiring the interval to be updated? The interval is important,
-- at least in the case of the sphere, because it's used to determine which root to return.
hit ::
  (Floating a, Ord a) =>
  Ray a ->
  Interval2D a ->
  World [Sphere a] ->
  Maybe (HitRecord a)
hit _ _ (World []) = Nothing
hit ray interval (World objects) = fst $ foldl go (Nothing, interval) objects
  where
    go (currentRecord, currentInterval) object = case Sphere.hit ray currentInterval object of
      Nothing -> (currentRecord, currentInterval)
      Just newRecord -> (Just newRecord, Interval2D {min = currentInterval.min, max = newRecord.t})
