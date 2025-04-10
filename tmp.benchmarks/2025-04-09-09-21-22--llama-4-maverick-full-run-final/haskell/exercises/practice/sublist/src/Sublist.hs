module Sublist (sublist) where

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys  = Just EQ
  | isSublist xs ys = Just LT
  | isSublist ys xs = Just GT
  | otherwise   = Nothing

isSublist :: Eq a => [a] -> [a] -> Bool
isSublist [] _ = True
isSublist _ [] = False
isSublist xs@(x:xt) (y:yt)
  | x == y    = xs `isPrefixOf` (y:yt) || isSublist xs yt
  | otherwise = isSublist xs yt

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
