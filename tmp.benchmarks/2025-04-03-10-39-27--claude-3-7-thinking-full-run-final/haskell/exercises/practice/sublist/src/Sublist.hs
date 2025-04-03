module Sublist (sublist) where

-- Check if xs is a contiguous subsequence of ys
isSubsequence :: Eq a => [a] -> [a] -> Bool
isSubsequence [] _ = True  -- An empty list is a subsequence of any list
isSubsequence _ [] = False  -- A non-empty list is not a subsequence of an empty list
isSubsequence xs ys@(_:ys') = isPrefixOf xs ys || isSubsequence xs ys'

-- Check if xs is a prefix of ys
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True  -- An empty list is a prefix of any list
isPrefixOf (_:_) [] = False  -- A non-empty list is not a prefix of an empty list
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys 
  | xs == ys = Just EQ  -- If both lists are equal
  | isSubsequence xs ys = Just LT  -- If xs is a sublist of ys
  | isSubsequence ys xs = Just GT  -- If xs is a superlist of ys
  | otherwise = Nothing  -- If none of the above
