module Viewport where

import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Ratio ((%))
import GHC.Records
import Point
import Triplet
import Vector
import Prelude

data ViewportConfig (a :: Type) where
  ViewportConfig ::
    { -- | The width of the viewport in pixels.
      aspectWidth :: Integer,
      -- | The height of the viewport in pixels.
      aspectHeight :: Integer,
      -- | The ratio of width to height.
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

mkViewportConfig :: (Num a, Floating a) => Integer -> Integer -> Integer -> ViewportConfig a
mkViewportConfig aspectWidth aspectHeight imageWidth =
  let -- Core values
      aspectRatio = aspectWidth % aspectHeight
      imageHeight = floor $ (imageWidth % 1) * aspectRatio
      cameraCenter = pure 0

      -- Determine viewport dimensions
      focalLength = 1.0
      viewportHeight = 2.0 * focalLength
      viewportWidth = (fromRational aspectRatio) * viewportHeight

      -- Calculate viewport location
      viewportU = Vector $ Triplet (viewportWidth, 0.0, 0.0)
      viewportV = Vector $ Triplet (0.0, -viewportHeight, 0.0)
      viewportAvg = (recip 2.0) `Vector.scale` (viewportU + viewportV)

      -- Calculate the delta values from pixel to pixel
      deltaU = (recip $ fromIntegral imageWidth) `Vector.scale` viewportU
      deltaV = (recip $ fromIntegral imageHeight) `Vector.scale` viewportV
      deltaAvg = (recip 2.0) `Vector.scale` (deltaU + deltaV)

      -- Calculate the location of the upper-left pixel
      viewportUpperLeft = cameraCenter - (coerce viewportAvg) - (Point $ Triplet (0.0, 0.0, focalLength))
      loc00 = viewportUpperLeft + (coerce deltaAvg)
   in ViewportConfig {..}

-- | Get the center of the pixel at x, y.
getPixelCenter :: (Num a, Floating a) => ViewportConfig a -> Integer -> Integer -> Point a
getPixelCenter cfg x y =
  (fromIntegral y) `Point.scale` (coerce cfg.deltaV) + (fromIntegral x) `Point.scale` (coerce cfg.deltaU) + cfg.loc00

-- | Get the random point in the square surrounding a pixel at the origin.
samplePixelSquare :: (Num a, Floating a) => ViewportConfig a -> Point a
samplePixelSquare cfg =
  let px = -0.5 * undefined
      py = -0.5 * undefined
   in py `Point.scale` (coerce cfg.deltaV) + px `Point.scale` (coerce cfg.deltaU)
