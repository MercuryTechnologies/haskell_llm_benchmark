module Sublist (sublist) where

import Data.Maybe (isJust)

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
    | xs == ys = Just EQ
    | isSublist xs ys = Just LT
    | isSublist ys xs = Just GT
    | otherwise = Nothing

isSublist :: Eq a => [a] -> [a] -> Bool
isSublist [] _ = True
isSublist _ [] = False
isSublist xs@(x:xt) ys@(y:yt)
    | xs == take (length xs) ys = True
    | otherwise = isSublist xs yt
