module Main (main) where

import Camera (CameraConfig (..), render)
import Color (Color, showColorAsPPM)
import Data.Random (RVar, runRVar)
import Data.String (IsString (fromString))
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import GHC.Records (HasField (getField))
import Point (Point (Point))
import Renderer (defaultRenderConfig)
import Sphere (Sphere (Sphere))
import System.Random.MWC (initialize)
import Triplet (Triplet (..))
import Viewport
  ( ViewportConfig (imageHeight, imageWidth),
    defaultViewportConfig,
  )
import World (World (..))
import Prelude

cameraConfig :: CameraConfig Double
cameraConfig = CameraConfig {renderer = defaultRenderConfig, viewport = defaultViewportConfig}

defaultWorld :: World (Sphere Double)
defaultWorld =
  World
    [ Sphere (Point $ Triplet (0.0, -100.5, -1.0)) 100.0,
      Sphere (Point $ Triplet (0.0, 0.0, -1.0)) 0.5
    ]

-- NOTE: Resulting vector of vectors is indexed by (height, width)
defaultRender :: RVar (V.Vector (V.Vector (Color Double)))
defaultRender = render cameraConfig defaultWorld

main :: IO ()
main = do
  rand <- initialize (VS.singleton 2021)
  result <- runRVar defaultRender rand
  let asPPM = fmap (fmap showColorAsPPM) result
  writeFile "image.ppm" . unlines $
    [ "P3",
      show cameraConfig.viewport.imageWidth <> " " <> show cameraConfig.viewport.imageHeight,
      "255"
    ]
      -- Correct the indexing
      <> [ asPPM V.! h V.! w
           | h <- [0 .. (fromInteger cameraConfig.viewport.imageHeight - 1)],
             w <- [0 .. (fromInteger cameraConfig.viewport.imageWidth - 1)]
         ]
