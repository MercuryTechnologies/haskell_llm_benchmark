module Sublist (sublist) where

import Data.List (isPrefixOf, tails)

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
    | xs == ys                = Just EQ
    | isContained xs ys       = Just LT
    | isContained ys xs       = Just GT
    | otherwise               = Nothing
  where
    isContained small big = any (small `isPrefixOf`) (tails big)
