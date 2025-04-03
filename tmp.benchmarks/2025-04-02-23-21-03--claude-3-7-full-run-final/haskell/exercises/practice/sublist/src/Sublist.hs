module Sublist (sublist) where

import Data.List (isInfixOf)

-- | Determine if the first list is a sublist, superlist, equal to, or none of these
-- compared to the second list.
sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist [] [] = Just EQ  -- Both empty lists are equal
sublist [] _  = Just LT  -- Empty list is a sublist of any non-empty list
sublist _  [] = Just GT  -- Any non-empty list is a superlist of an empty list
sublist xs ys
  | xs == ys        = Just EQ  -- Lists are equal
  | isSubsequence xs ys = Just LT  -- First list is a sublist of second
  | isSubsequence ys xs = Just GT  -- First list is a superlist of second
  | otherwise       = Nothing  -- Lists are unequal

-- | Check if the first list appears as a contiguous subsequence of the second list
isSubsequence :: Eq a => [a] -> [a] -> Bool
isSubsequence xs ys = isInfixOf xs ys
