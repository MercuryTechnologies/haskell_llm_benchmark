module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p [] = []
discard p (x:xs)
  | p x       = discard p xs
  | otherwise = x : discard p xs

keep :: (a -> Bool) -> [a] -> [a]
keep p [] = []
keep p (x:xs)
  | p x       = x : keep p xs
  | otherwise = keep p xs
