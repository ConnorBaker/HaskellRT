module Point (Point (..), scale) where

import Data.Kind (Type)
import Triplet (Triplet)
import Prelude

-- | A point in space.
newtype Point (a :: Type) where
  Point :: Triplet a -> Point a
  deriving newtype (Eq, Ord, Show, Num, Fractional, Floating, Functor, Applicative, Foldable)

scale :: (Num a) => a -> Point a -> Point a
scale = (*) . pure
