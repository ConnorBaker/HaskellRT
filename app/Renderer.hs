module Renderer where

import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Random (Distribution, RVar, StdUniform)
import GHC.Records (HasField (getField))
import HitRecord (HitRecord (normal, point))
import Interval2D (Interval2D (Interval2D))
import Ray (Ray (..))
import UnitVector
  ( UnitVector (UnitVector),
    mkUnitVector,
    randomUniformUnitVectorOnUnitHemisphere,
  )
import Vector (Vector (Vector), sampleVectorStdUniform)
import Prelude

data RendererConfig (a :: Type) where
  RendererConfig ::
    { -- | Number of samples per pixel; minimum of one.
      -- Higher values enable anti-aliasing.
      samplesPerPixel :: Integer,
      -- | Maximum number of bounces for a single ray.
      maxDepth :: Integer,
      -- | The interval of distances to check for hits.
      -- Recommended to be non-zero to avoid self-intersection (which causes "shadow acne").
      hitInterval :: Interval2D a,
      -- | Whether to use Lambertian diffuse shading.
      useLambertian :: Bool
    } ->
    RendererConfig a
  deriving (Eq, Show)

defaultRenderConfig :: (Floating a) => RendererConfig a
defaultRenderConfig =
  RendererConfig
    { samplesPerPixel = 1,
      maxDepth = 10,
      hitInterval = Interval2D 1e-9 1e10,
      useLambertian = False
    }

-- | Gets a randomly sampled diffuse ray from the hit point.
-- This is a uniform sampling.
getDiffuseRayUniform :: forall a. (Ord a, Floating a, Distribution StdUniform a) => HitRecord a -> RVar (Ray a)
getDiffuseRayUniform hitRecord = do
  direction <- randomUniformUnitVectorOnUnitHemisphere hitRecord.normal
  pure $ Ray {origin = hitRecord.point, ..}

-- | Gets a diffuse ray from the hit point using a non-uniform Lambertian sampling.
getDiffuseRayLambertian :: (Ord a, Floating a, Distribution StdUniform a) => HitRecord a -> RVar (Ray a)
getDiffuseRayLambertian hitRecord = do
  v <- sampleVectorStdUniform
  case mkUnitVector (v + coerce hitRecord.normal) of
    Nothing -> getDiffuseRayLambertian hitRecord
    Just direction -> do
      pure $ Ray {origin = hitRecord.point, ..}
