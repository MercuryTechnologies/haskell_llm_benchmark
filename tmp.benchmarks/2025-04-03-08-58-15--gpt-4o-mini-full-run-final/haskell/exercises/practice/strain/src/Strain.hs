module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = foldr (\x acc -> if not (p x) then x : acc else acc) [] xs

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs
