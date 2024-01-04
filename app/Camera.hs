module Camera where

import Color (Color, scale, skyFn)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Random (Distribution, RVar, StdUniform)
import Data.String (IsString (fromString))
import Data.Vector qualified as V
import GHC.Records (HasField (getField))
import HitRecord (HitRecord)
import Hittable (Hittable)
import Point (Point (..))
import Ray (Ray (..))
import Renderer
  ( RendererConfig
      ( hitInterval,
        maxDepth,
        samplesPerPixel,
        useLambertian
      ),
    getDiffuseRayLambertian,
    getDiffuseRayUniform,
  )
import Triplet (Triplet (Triplet))
import UnitVector (UnitVector, mkUnitVector)
import Vector (Vector (Vector))
import Viewport
  ( ViewportConfig (cameraCenter, imageHeight, imageWidth),
    getPixelCenter,
  )
import World (World, hit)
import Prelude

data CameraConfig (a :: Type) where
  CameraConfig ::
    { renderer :: RendererConfig a,
      viewport :: ViewportConfig a
    } ->
    CameraConfig a
  deriving (Eq, Show)

type GetPixelFn (h :: Type -> Type) (a :: Type) = CameraConfig a -> CameraMethods h a -> World (h a) -> Point a -> RVar (Color a)

type DiffuseRayScatterFn (a :: Type) = HitRecord a -> RVar (Ray a)

data CameraMethods (h :: Type -> Type) (a :: Type) where
  CameraMethods ::
    { -- | Function to get the color of a pixel.
      getPixelFn :: GetPixelFn h a,
      -- | Function to get a diffuse ray from a hit record.
      diffuseRayScatterFn :: DiffuseRayScatterFn a
    } ->
    CameraMethods h a

mkCameraMethods ::
  forall (h :: Type -> Type) (a :: Type).
  (Ord a, Floating a, Distribution StdUniform a, Hittable h a) =>
  CameraConfig a ->
  CameraMethods h a
mkCameraMethods cfg =
  let getPixelFn :: GetPixelFn h a
      getPixelFn = case cfg.renderer.samplesPerPixel of
        1 -> pixelNoFilter
        _ -> pixelBoxFilter
      diffuseRayScatterFn :: DiffuseRayScatterFn a
      diffuseRayScatterFn = case cfg.renderer.useLambertian of
        False -> getDiffuseRayUniform
        True -> getDiffuseRayLambertian
   in CameraMethods {..}

-- | Renders the world to a vector of vectors of colors, indexed by (height, width).
render ::
  forall (h :: Type -> Type) (a :: Type).
  (Ord a, Floating a, Distribution StdUniform a, Hittable h a) =>
  -- | The camera configuration.
  CameraConfig a ->
  -- | The world.
  World (h a) ->
  -- | The vector of vectors of colors, indexed by (height, width).
  RVar (V.Vector (V.Vector (Color a)))
render cfg world =
  let h :: Int
      h = fromInteger cfg.viewport.imageHeight
      w :: Int
      w = fromInteger cfg.viewport.imageWidth
      mth :: CameraMethods h a
      mth = mkCameraMethods cfg
   in sequenceA . V.generate h $
        ( \y ->
            sequenceA . V.generate w $
              (\x -> getPixelColor cfg mth world (fromIntegral x) (fromIntegral y))
        )

-- | Gets the color of a pixel.
getPixelColor ::
  forall (h :: Type -> Type) (a :: Type).
  (Ord a, Floating a, Distribution StdUniform a, Hittable h a) =>
  -- | The camera configuration.
  CameraConfig a ->
  -- | The camera methods.
  CameraMethods h a ->
  -- | The world.
  World (h a) ->
  -- | The x coordinate of the pixel.
  Integer ->
  -- | The y coordinate of the pixel.
  Integer ->
  -- | The color of the pixel.
  RVar (Color a)
getPixelColor cfg mth world x y
  | x < 0 = error "x is negative."
  | x >= cfg.viewport.imageWidth = error "x is too large."
  | y < 0 = error "y is negative."
  | y >= cfg.viewport.imageHeight = error "y is too large."
  | cfg.renderer.samplesPerPixel < 1 = error "samplesPerPixel is less than 1."
  | otherwise = mth.getPixelFn cfg mth world (getPixelCenter cfg.viewport x y)

-- | Gets the color of a pixel using no filtering.
pixelNoFilter ::
  forall (h :: Type -> Type) (a :: Type).
  (Ord a, Floating a, Distribution StdUniform a, Hittable h a) =>
  -- | The camera configuration.
  CameraConfig a ->
  -- | The camera methods.
  CameraMethods h a ->
  -- | The world.
  World (h a) ->
  -- | The center of the pixel.
  Point a ->
  -- | The color of the pixel.
  RVar (Color a)
pixelNoFilter cfg mth world pixelCenter =
  let origin :: Point a
      origin = cfg.viewport.cameraCenter
      direction :: UnitVector a
      direction = case mkUnitVector (coerce (pixelCenter - origin)) of
        Nothing -> error "Camera direction is zero vector."
        Just v -> v
   in getRayColor cfg mth world (pure Ray {..})

-- | Gets the color of a pixel using a box filter.
pixelBoxFilter ::
  forall (h :: Type -> Type) (a :: Type).
  (Ord a, Floating a, Distribution StdUniform a, Hittable h a) =>
  -- | The camera configuration.
  CameraConfig a ->
  -- | The camera methods.
  CameraMethods h a ->
  -- | The world.
  World (h a) ->
  -- | The center of the pixel.
  Point a ->
  -- | The color of the pixel.
  RVar (Color a)
pixelBoxFilter = undefined

-- | Gets the color of a ray.
getRayColor ::
  forall (h :: Type -> Type) (a :: Type).
  (Ord a, Floating a, Distribution StdUniform a, Hittable h a) =>
  -- | The camera configuration.
  CameraConfig a ->
  -- | The camera methods.
  CameraMethods h a ->
  -- | The world.
  World (h a) ->
  -- | The ray.
  RVar (Ray a) ->
  -- | The color of the ray.
  RVar (Color a)
getRayColor cfg mth world ray =
  let rayDecision :: RVar (Ray a) -> Integer -> a -> RVar (Color a)
      rayDecision currentRay' currentDepth currentLightAttenuation =
        case currentDepth < cfg.renderer.maxDepth of
          -- False -> pure black
          -- TODO: Black image entirely when using pure black -- to few bounces allowed?
          False -> skyFn <$> currentRay'
          True -> do
            currentRay <- currentRay'
            case World.hit world cfg.renderer.hitInterval currentRay of
              Nothing -> pure $ currentLightAttenuation `Color.scale` skyFn currentRay
              Just hitRecord -> rayDecision (mth.diffuseRayScatterFn hitRecord) (currentDepth + 1) (currentLightAttenuation / 2.0)
   in rayDecision ray 0 1.0
