module World where

import Data.Kind (Type)
import GHC.Records (HasField (getField))
import HitRecord (HitRecord (t))
import Hittable (Hittable (..))
import Interval2D (Interval2D (..))
import Ray (Ray)
import Prelude

-- | A world containing a list of objects.
newtype World (a :: Type) where
  World :: [a] -> World a
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
  forall (h :: Type -> Type) (a :: Type).
  (Hittable h a) =>
  -- | The world containing the objects.
  World (h a) ->
  -- | The interval to search for hits.
  Interval2D a ->
  -- | The ray to check for hits.
  Ray a ->
  -- | The hit record, if any.
  Maybe (HitRecord a)
hit (World []) _ _ = Nothing
hit (World objects) interval ray = fst $ foldl go (Nothing, interval) objects
  where
    go :: (Maybe (HitRecord a), Interval2D a) -> h a -> (Maybe (HitRecord a), Interval2D a)
    go (currentRecord, currentInterval) object =
      case Hittable.hit object currentInterval ray of
        Nothing -> (currentRecord, currentInterval)
        Just newRecord -> (Just newRecord, Interval2D {min = currentInterval.min, max = newRecord.t})
