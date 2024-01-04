module Hittable where

import Data.Kind (Type)
import HitRecord (HitRecord)
import Interval2D (Interval2D)
import Ray (Ray)
import Prelude

class Hittable (h :: Type -> Type) (a :: Type) where
  hit ::
    h a ->
    Interval2D a ->
    Ray a ->
    Maybe (HitRecord a)
