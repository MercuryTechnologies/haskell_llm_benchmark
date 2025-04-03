module Sublist (sublist) where

import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Function (on)

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys    = Just EQ
  | xs `isInfixOf` ys = Just LT
  | ys `isInfixOf` xs = Just GT
  | otherwise   = Nothing
