module Sublist (sublist) where

import Data.List (isInfixOf)

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist a b
  | a == b          = Just EQ
  | a `isInfixOf` b = Just LT
  | b `isInfixOf` a = Just GT
  | otherwise       = Nothing
