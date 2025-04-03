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

-- We store our CustomSet as a sorted list without duplicates.
newtype CustomSet a = CustomSet [a]
  deriving (Show, Eq)

-- | Create an empty set.
empty :: CustomSet a
empty = CustomSet []

-- | Build a set from a list of elements.
fromList :: Ord a => [a] -> CustomSet a
fromList = CustomSet . canonicalize

-- Helper to ensure a list is sorted and has no duplicates.
canonicalize :: Ord a => [a] -> [a]
canonicalize = L.nub . L.sort

-- | Remove an element from a set if it exists.
delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (L.delete x xs)

-- | Return the elements in setA that don't appear in setB.
difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) = CustomSet (xs L.\\ ys)

-- | Insert an element into a set.
insert :: Ord a => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs)
  | x `elem` xs = CustomSet xs
  | otherwise   = CustomSet (L.insert x xs)

-- | Return the intersection of two sets.
intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) = CustomSet (xs `L.intersect` ys)

-- | Check if two sets have no elements in common.
isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CustomSet xs) (CustomSet ys) = L.null (xs `L.intersect` ys)

-- | Check if setA is a subset of setB.
isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) (CustomSet ys) = all (`elem` ys) xs

-- | Check if an element is a member of a set.
member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

-- | Test if a set is empty.
null :: CustomSet a -> Bool
null (CustomSet xs) = L.null xs

-- | Get the size of a set.
size :: CustomSet a -> Int
size (CustomSet xs) = length xs

-- | Convert a set to a list.
toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

-- | Return the union of two sets.
union :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = CustomSet (canonicalize (xs ++ ys))
