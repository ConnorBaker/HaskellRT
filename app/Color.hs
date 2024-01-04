module Color where

import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Ord (clamp)
import GHC.Records (HasField (getField))
import Ray (Ray (direction))
import Triplet (Triplet (..))
import UnitVector (UnitVector (UnitVector))
import Vector (Vector (Vector))
import Prelude

-- | A color in RGB space.
newtype Color (a :: Type) where
  Color :: Triplet a -> Color a
  deriving newtype (Eq, Ord, Show, Num, Fractional, Floating, Functor, Applicative, Foldable)

scale :: (Num a) => a -> Color a -> Color a
scale = (*) . pure

black :: (Ord a, Floating a) => Color a
black = Color (pure 0)

white :: (Ord a, Floating a) => Color a
white = Color (pure 1)

skyBlue :: (Ord a, Floating a) => Color a
skyBlue = Color (Triplet (0.5, 0.7, 1))

-- | Returns a gradient background sky background color.
--
-- Use a linear blend:
--     blended_value = (1 - a) * start_value + a * end_value
--     blended_value = start_value - a * start_value + a * end_value
--     blended_value = start_value + a * (end_value - start_value)
--     blended_value = fma(a, end_value - start_value, start_value)
-- Also known as a linear interpolation or "lerp".
skyFn :: forall a. (Ord a, Floating a) => Ray a -> Color a
skyFn ray =
  let directionY :: a
      directionY = (\(Triplet (_, y, _)) -> y) (coerce ray.direction)

      gradient :: Color a
      gradient = (((directionY + 1) / 2.0) `Color.scale` (skyBlue - white)) + white
   in gradient

showColorAsPPM :: forall a. (Ord a, Floating a, RealFrac a, Show a) => Color a -> String
showColorAsPPM floatColor =
  let stringColor :: Color String
      stringColor = fmap (show @Int . floor . (* 255) . clamp (0.0, 1.0)) floatColor

      ppmString = (\(r, g, b) -> unwords [r, g, b]) (coerce stringColor)
   in ppmString
