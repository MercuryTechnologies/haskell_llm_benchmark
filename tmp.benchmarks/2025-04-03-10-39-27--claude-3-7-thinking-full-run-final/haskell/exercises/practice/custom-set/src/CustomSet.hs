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

-- Define our CustomSet type as a newtype wrapping a list
newtype CustomSet a = CustomSet { unCustomSet :: [a] }
  deriving (Show)

-- Define equality - two sets are equal if they contain the same elements
instance Eq a => Eq (CustomSet a) where
  (CustomSet xs) == (CustomSet ys) = 
    length xs == length ys && all (`elem` ys) xs

-- Create an empty set
empty :: CustomSet a
empty = CustomSet []

-- Convert a list to a set, eliminating duplicates
fromList :: Eq a => [a] -> CustomSet a
fromList = CustomSet . foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- Convert a set back to a list
toList :: CustomSet a -> [a]
toList = unCustomSet

-- Check if a set is empty
null :: CustomSet a -> Bool
null (CustomSet xs) = length xs == 0

-- Get the number of elements in a set
size :: CustomSet a -> Int
size (CustomSet xs) = length xs

-- Check if an element is in the set
member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

-- Insert an element into a set
insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x set
  | member x set = set
  | otherwise = CustomSet (x : toList set)

-- Delete an element from a set
delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (filter (/= x) xs)

-- Union of two sets
union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = fromList (xs ++ ys)

-- Intersection of two sets
intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) = CustomSet (filter (`elem` ys) xs)

-- Difference of two sets (elements in first but not in second)
difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) = CustomSet (filter (`notElem` ys) xs)

-- Check if two sets are disjoint (no common elements)
isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CustomSet xs) (CustomSet ys) = not (any (`elem` ys) xs)

-- Check if first set is a subset of the second
isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) (CustomSet ys) = all (`elem` ys) xs
