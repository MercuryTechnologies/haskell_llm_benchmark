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

import Prelude hiding (null)
import qualified Data.List as List

newtype CustomSet a = CustomSet [a]

instance (Eq a, Show a) => Show (CustomSet a) where
  show (CustomSet xs) = "CustomSet " ++ show (List.nub xs)

instance Eq a => Eq (CustomSet a) where
  CustomSet xs == CustomSet ys = List.nub xs == List.nub ys

empty :: CustomSet a
empty = CustomSet []

fromList :: Eq a => [a] -> CustomSet a
fromList xs = CustomSet (foldr (\x acc -> if x `elem` acc then acc else x : acc) [] xs)

toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs)
  | x `elem` xs = CustomSet xs
  | otherwise = CustomSet (x : xs)

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (filter (/= x) xs)

member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

null :: CustomSet a -> Bool
null (CustomSet xs) = List.null xs

size :: CustomSet a -> Int
size (CustomSet xs) = length (List.nub xs)

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = fromList (xs ++ ys)

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) = CustomSet (filter (`elem` ys) xs)

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) = CustomSet (filter (`notElem` ys) xs)

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) (CustomSet ys) = all (`elem` ys) xs

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CustomSet xs) (CustomSet ys) = all (`notElem` ys) xs
