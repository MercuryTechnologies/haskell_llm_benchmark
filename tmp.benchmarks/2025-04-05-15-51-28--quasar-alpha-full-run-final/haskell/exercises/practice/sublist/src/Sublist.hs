module Sublist (sublist) where

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys = Just EQ
  | isSublist xs ys = Just LT
  | isSublist ys xs = Just GT
  | otherwise = Nothing

isSublist :: Eq a => [a] -> [a] -> Bool
isSublist [] _ = True
isSublist _ [] = False
isSublist needle haystack@(h:hs)
  | prefixMatch needle haystack = True
  | otherwise = isSublist needle hs

prefixMatch :: Eq a => [a] -> [a] -> Bool
prefixMatch [] _ = True
prefixMatch _ [] = False
prefixMatch (x:xs) (y:ys) = x == y && prefixMatch xs ys
