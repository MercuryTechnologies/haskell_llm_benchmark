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

-- foldl' implementation
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z []     = z
foldl' f z (x:xs) = let z' = z `f` x
                    in seq z' $ foldl' f z' xs

-- foldr implementation
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = x `f` foldr f z xs

-- length implementation
length :: [a] -> Int
length = foldl' (\acc _ -> acc + 1) 0

-- reverse implementation
reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

-- map implementation
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x acc -> f x : acc) []

-- filter implementation
filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x acc -> if p x then x : acc else acc) []

-- (++) implementation
(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

-- concat implementation
concat :: [[a]] -> [a]
concat = foldr (++) []
