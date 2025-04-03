module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )
import qualified Prelude

-- Strict left fold.
-- foldl' f acc []     = acc
-- foldl' f acc (x:xs) = let acc' = f acc x in acc' `seq` foldl' f acc' xs
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x:xs) =
  let acc' = f acc x
  in acc' `seq` foldl' f acc' xs

-- Right fold.
-- foldr f z []     = z
-- foldr f z (x:xs) = f x (foldr f z xs)
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

-- Calculate the length of a list.
-- length = foldl' (\n _ -> n + 1) 0
length :: [a] -> Int
length = foldl' (\n _ -> n + 1) 0

-- Reverse a list.
-- reverse = foldl' (\acc x -> x : acc) []
reverse :: [a] -> [a]
reverse = foldl' (\acc x -> x : acc) []

-- Apply a function to each element of a list.
-- map f = foldr (\x acc -> f x : acc) []
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x acc -> f x : acc) []

-- Filter list elements based on a predicate.
-- filter p = foldr (\x acc -> if p x then x : acc else acc) []
filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x acc -> if p x then x : acc else acc) []

-- Append two lists.
-- [] ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- Concatenate a list of lists.
-- concat = foldr (++) []
concat :: [[a]] -> [a]
concat = foldr (++) []
