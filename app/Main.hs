module Main (main) where

import Control.Monad (join)
import Data.String (IsString (fromString))
import Triplet (Triplet (..))
import Vector (Vector (..))
import Prelude

v1 :: (Floating a) => Vector a
v1 = Vector $ Triplet (1, 2, 3)

v2 :: (Floating a) => Vector a
v2 = Vector $ Triplet (4.0, 5.0, 6.0)

main :: IO ()
main = do
  putStrLn $ "v1: " <> show v1
  putStrLn $ "v2: " <> show v2
  putStrLn $ "v1 == v2: " <> show (v1 == v2)
  putStrLn $ "v1 + v2: " <> show (v1 + v2)
  putStrLn $ "v1 - v2: " <> show (v1 - v2)
  putStrLn $ "v1 * v2: " <> show (v1 * v2)
  putStrLn $ "v1 / v2: " <> show (v1 / v2)
  putStrLn "Hello, Haskell!"
