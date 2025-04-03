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
import qualified Data.List as L

-- Define CustomSet as a newtype wrapper around a list of elements 'a'.
-- The internal list is kept sorted and without duplicates.
-- This requires the element type 'a' to be an instance of the Ord typeclass
-- for most operations.
newtype CustomSet a = CustomSet { unSet :: [a] } deriving (Show)

-- Two sets are equal if their underlying sorted lists of unique elements are equal.
instance (Ord a) => Eq (CustomSet a) where
  (CustomSet xs) == (CustomSet ys) = xs == ys

-- Deletes an element from the set.
-- If the element is not in the set, the set remains unchanged.
delete :: Ord a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (delete' xs)
  where
    delete' [] = []
    delete' (y:ys)
      | x < y     = y:ys -- x is not in the set (since list is sorted)
      | x == y    = ys   -- Found x, return the rest of the list
      | otherwise = y : delete' ys -- x > y, keep searching

-- Returns the difference of two sets (elements in the first set but not in the second).
difference :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) = CustomSet (diff xs ys)
  where
    -- Efficient difference for sorted lists using a merge-like process.
    diff [] _ = []
    diff xss [] = xss
    diff (x:xt) (y:yt)
      | x < y     = x : diff xt (y:yt) -- x is unique to the first list
      | x == y    = diff xt yt         -- x is in both, discard it
      | otherwise = diff (x:xt) yt     -- y < x, discard y from the second list and keep checking x

-- Returns an empty set.
empty :: CustomSet a
empty = CustomSet []

-- Creates a set from a list of elements. Duplicates are removed, and the
-- internal list is sorted.
fromList :: Ord a => [a] -> CustomSet a
fromList = CustomSet . L.nub . L.sort

-- Inserts an element into the set.
-- If the element is already present, the set remains unchanged.
insert :: Ord a => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs) = CustomSet (insert' xs)
  where
    -- Efficient insertion into a sorted list, maintaining order and uniqueness.
    insert' [] = [x]
    insert' (y:ys)
      | x < y     = x : y : ys -- Insert x before y
      | x == y    = y : ys     -- x is already in the set
      | otherwise = y : insert' ys -- x > y, keep searching

-- Returns the intersection of two sets (elements common to both).
intersection :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) = CustomSet (intersect' xs ys)
  where
    -- Efficient intersection for sorted lists using a merge-like process.
    intersect' [] _ = []
    intersect' _ [] = []
    intersect' (x:xt) (y:yt)
      | x < y     = intersect' xt (y:yt) -- Discard x
      | x == y    = x : intersect' xt yt -- Found common element, include it
      | otherwise = intersect' (x:xt) yt -- y < x, discard y

-- Checks if two sets are disjoint (have no elements in common).
isDisjointFrom :: Ord a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CustomSet xs) (CustomSet ys) = disjoint' xs ys
  where
    -- Efficient check for disjointness using a merge-like process.
    disjoint' [] _ = True
    disjoint' _ [] = True
    disjoint' (x:xt) (y:yt)
      | x < y     = disjoint' xt (y:yt)
      | x == y    = False -- Found common element
      | otherwise = disjoint' (x:xt) yt -- y < x

-- Checks if the first set is a subset of the second set
-- (all elements of the first set are present in the second).
isSubsetOf :: Ord a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) (CustomSet ys) = subset' xs ys
  where
    -- Efficient check for subset relation using a merge-like process.
    subset' [] _ = True  -- Empty set is a subset of any set
    subset' _ [] = False -- Non-empty set cannot be a subset of an empty set
    subset' (x:xt) (y:yt)
      | x < y     = False -- Element x from set1 is not in set2
      | x == y    = subset' xt yt -- Found common element, check rest of set1
      | otherwise = subset' (x:xt) yt -- y < x, advance set2 to find x

-- Checks if an element is a member of the set.
member :: Ord a => a -> CustomSet a -> Bool
member x (CustomSet xs) = member' xs
  where
    -- Efficient check using the sorted property (similar to binary search logic).
    member' [] = False
    member' (y:ys)
      | x < y     = False -- Element cannot be in the rest of the list
      | x == y    = True
      | otherwise = member' ys -- x > y, keep searching

-- Checks if the set is empty.
null :: CustomSet a -> Bool
null (CustomSet xs) = L.null xs

-- Returns the number of elements in the set.
size :: CustomSet a -> Int
size (CustomSet xs) = length xs

-- Converts the set to a list of elements in ascending order.
toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

-- Returns the union of two sets (all unique elements from both sets).
union :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = CustomSet (union' xs ys)
  where
    -- Efficient union for sorted lists using a merge-like process.
    union' [] ys' = ys'
    union' xs' [] = xs'
    union' (x:xt) (y:yt)
      | x < y     = x : union' xt (y:yt)
      | x == y    = x : union' xt yt -- Include element once, advance both
      | otherwise = y : union' (x:xt) yt -- y < x
