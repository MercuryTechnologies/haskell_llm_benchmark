module CustomSet
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import qualified Data.List as L
import Prelude hiding (null)

newtype CustomSet a = CS { unCS :: [a] }

-- Define equality in an order-independent way
instance (Eq a) => Eq (CustomSet a) where
  (CS xs) == (CS ys) = length xs == length ys && all (`elem` ys) xs

instance (Show a) => Show (CustomSet a) where
  show set = "CustomSet " ++ show (toList set)

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CS xs) = CS (filter (/= x) xs)

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CS xs) (CS ys) = CS (filter (`notElem` ys) xs)

empty :: CustomSet a
empty = CS []

fromList :: Eq a => [a] -> CustomSet a
fromList xs = CS (foldr (\x acc -> if x `elem` acc then acc else x:acc) [] xs)

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x (CS xs)
  | x `elem` xs = CS xs
  | otherwise   = CS (x : xs)

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CS xs) (CS ys) = CS (filter (`elem` ys) xs)

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = null (intersection setA setB)

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CS xs) (CS ys) = all (`elem` ys) xs

member :: Eq a => a -> CustomSet a -> Bool
member x (CS xs) = x `elem` xs

null :: CustomSet a -> Bool
null (CS xs) = L.null xs

size :: CustomSet a -> Int
size (CS xs) = length xs

toList :: CustomSet a -> [a]
toList (CS xs) = xs

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union (CS xs) (CS ys) = CS (xs ++ filter (`notElem` xs) ys)
