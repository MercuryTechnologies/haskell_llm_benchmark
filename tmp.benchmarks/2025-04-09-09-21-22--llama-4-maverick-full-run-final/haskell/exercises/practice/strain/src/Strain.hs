module Strain (keep, discard) where

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep p (x:xs)
  | p x       = x : keep p xs
  | otherwise = keep p xs

discard :: (a -> Bool) -> [a] -> [a]
discard _ [] = []
discard p (x:xs)
  | p x       = discard p xs
  | otherwise = x : discard p xs
