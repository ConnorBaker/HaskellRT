{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interval2D (Interval2D (..), mkInterval2D, contains, surrounds) where

import Control.Monad (guard)
import Data.Kind (Type)
import Prelude

data Interval2D (a :: Type) where
  Interval2D ::
    { -- | The minimum value of the interval.
      min :: a,
      -- | The maximum value of the interval.
      max :: a
    } ->
    Interval2D a
  deriving (Eq, Ord, Show)

-- | Create an 'Interval2D' from a minimum value and a maximum value.
mkInterval2D ::
  (Ord a) =>
  -- | The minimum value of the interval.
  a ->
  -- | The maximum value of the interval.
  a ->
  -- | The 'Interval2D' if the minimum value is less than or equal to the maximum value.
  Maybe (Interval2D a)
mkInterval2D min max = do
  guard (min <= max)
  pure Interval2D {..}

-- | Test if a value is contained in an 'Interval2D'.
contains ::
  (Ord a) =>
  -- | The 'Interval2D'.
  Interval2D a ->
  -- | The value.
  a ->
  -- | True if the value is contained in the 'Interval2D'.
  Bool
contains Interval2D {..} value = min <= value && value <= max

-- | Test if a value is surrounded by an 'Interval2D'.
surrounds ::
  (Ord a) =>
  -- | The 'Interval2D'.
  Interval2D a ->
  -- | The value.
  a ->
  -- | True if the value is surrounded by the 'Interval2D'.
  Bool
surrounds Interval2D {..} value = min < value && value < max
