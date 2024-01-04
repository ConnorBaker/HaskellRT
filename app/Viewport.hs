module Viewport where

import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Random (Distribution, RVar, StdUniform, stdUniform)
import Data.Ratio ((%))
import GHC.Records (HasField (getField))
import Point (Point (..), scale)
import Triplet (Triplet (Triplet))
import Vector (Vector (..), scale)
import Prelude

data ViewportConfig (a :: Type) where
  ViewportConfig ::
    { -- | The aspect ratio of the viewport.
      aspectRatio :: Rational,
      -- | The width of the image in pixels.
      imageWidth :: Integer,
      -- | The height of the image in pixels.
      imageHeight :: Integer,
      -- | Center of the camera.
      cameraCenter :: Point a,
      -- | Location of the top-left pixel.
      loc00 :: Point a,
      -- | Change in location of the pixel in the u direction.
      deltaU :: Vector a,
      -- | Change in location of the pixel in the v direction.
      deltaV :: Vector a
    } ->
    ViewportConfig a
  deriving (Eq, Show)

defaultViewportConfig :: (Floating a) => ViewportConfig a
defaultViewportConfig = mkViewportConfig (16 % 9) 400

mkViewportConfig ::
  forall a.
  (Floating a) =>
  -- | The aspect ratio of the viewport.
  Rational ->
  -- | The width of the image in pixels.
  Integer ->
  -- | The viewport configuration.
  ViewportConfig a
mkViewportConfig aspectRatio imageWidth =
  let -- Core values
      imageHeight :: Integer
      imageHeight = floor $ fromInteger imageWidth / aspectRatio
      cameraCenter :: Point a
      cameraCenter = pure 0

      -- Determine viewport dimensions
      focalLength :: a
      focalLength = 1.0
      viewportHeight :: a
      viewportHeight = 2.0 * focalLength
      viewportWidth :: a
      viewportWidth = fromRational aspectRatio * viewportHeight

      -- Calculate viewport location
      viewportU :: Vector a
      viewportU = Vector $ Triplet (viewportWidth, 0.0, 0.0)
      viewportV :: Vector a
      viewportV = Vector $ Triplet (0.0, -viewportHeight, 0.0)
      viewportAvg :: Vector a
      viewportAvg = recip 2.0 `Vector.scale` (viewportU + viewportV)

      -- Calculate the delta values from pixel to pixel
      deltaU :: Vector a
      deltaU = recip (fromIntegral imageWidth) `Vector.scale` viewportU
      deltaV :: Vector a
      deltaV = recip (fromIntegral imageHeight) `Vector.scale` viewportV
      deltaAvg :: Vector a
      deltaAvg = recip 2.0 `Vector.scale` (deltaU + deltaV)

      -- Calculate the location of the upper-left pixel
      viewportUpperLeft :: Point a
      viewportUpperLeft = cameraCenter - coerce viewportAvg - coerce (Triplet (0.0, 0.0, focalLength))
      loc00 :: Point a
      loc00 = viewportUpperLeft + coerce deltaAvg
   in ViewportConfig {..}

-- | Get the center of the pixel at x, y.
getPixelCenter ::
  (Num a, Floating a) =>
  -- | The viewport configuration.
  ViewportConfig a ->
  -- | The x coordinate of the pixel.
  Integer ->
  -- | The y coordinate of the pixel.
  Integer ->
  -- | The center of the pixel.
  Point a
getPixelCenter cfg x y =
  fromIntegral y `Point.scale` coerce cfg.deltaV + fromIntegral x `Point.scale` coerce cfg.deltaU + cfg.loc00

-- | Get the random point in the square surrounding a pixel at the origin.
samplePixelSquare ::
  (Floating a, Distribution StdUniform a) =>
  -- | The viewport configuration.
  ViewportConfig a ->
  -- | The random point in the square.
  RVar (Point a)
samplePixelSquare cfg = do
  px <- fmap (\p -> (-0.5) * p `Point.scale` coerce cfg.deltaV) stdUniform
  py <- fmap (\p -> (-0.5) * p `Point.scale` coerce cfg.deltaU) stdUniform
  pure $ px + py
