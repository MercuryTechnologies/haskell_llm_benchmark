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

-- Remove 'null' from the hiding clause to make Prelude.null available
import Prelude
import Data.List (sort, nub)
import qualified Data.List as L -- Use qualified import for clarity

-- Define CustomSet as a newtype wrapping a list of elements 'a'.
-- The list is kept sorted and unique internally.
-- Requires Ord constraint on 'a' for most operations.
newtype CustomSet a = CustomSet { unSet :: [a] } deriving (Show)

-- Two sets are equal if their internal sorted lists are equal.
-- Requires Ord a because the internal representation relies on order.
instance (Ord a) => Eq (CustomSet a) where
  (CustomSet xs) == (CustomSet ys) = xs == ys

-- Helper function to create a valid CustomSet (sorted, unique list)
-- from an arbitrary list. Requires Ord a for sorting and nub.
mkSet :: (Ord a) => [a] -> CustomSet a
mkSet = CustomSet . L.nub . L.sort

-- Deletes an element from the set. Maintains sorted order.
delete :: (Ord a) => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (delete' x xs)
  where
    delete' :: (Ord a) => a -> [a] -> [a]
    delete' _ [] = []
    delete' y (z:zs)
      | y < z     = z : zs -- Element not found (since list is sorted)
      | y == z    = zs     -- Element found, return rest of list
      | otherwise = z : delete' y zs -- Keep searching

-- Returns the difference of two sets (elements in the first set but not the second).
difference :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) = CustomSet (diff xs ys)
  where
    -- Both lists xs and ys are sorted and unique.
    diff :: (Ord a) => [a] -> [a] -> [a]
    diff [] _ = []
    diff xs' [] = xs'
    diff (x:xs') (y:ys')
      | x < y     = x : diff xs' (y:ys') -- x is not in ys, keep it
      | x == y    = diff xs' ys'         -- x is in ys, discard it from result
      | otherwise = diff (x:xs') ys'     -- y is smaller, discard y and check x against rest of ys

-- Returns an empty set.
empty :: CustomSet a
empty = CustomSet []

-- Creates a set from a list of elements.
fromList :: (Ord a) => [a] -> CustomSet a
fromList = mkSet -- Use helper to ensure sorted and unique properties

-- Inserts an element into the set. Maintains sorted order and uniqueness.
insert :: (Ord a) => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs) = CustomSet (insert' x xs)
  where
    insert' :: (Ord a) => a -> [a] -> [a]
    insert' y [] = [y]
    insert' y (z:zs)
      | y < z     = y : z : zs -- Insert y before z
      | y == z    = z : zs     -- Element already exists
      | otherwise = z : insert' y zs -- Keep searching for insertion point

-- Returns the intersection of two sets (elements common to both).
intersection :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) = CustomSet (intersect' xs ys)
  where
    -- Both lists xs and ys are sorted and unique.
    intersect' :: (Ord a) => [a] -> [a] -> [a]
    intersect' [] _ = []
    intersect' _ [] = []
    intersect' (x:xs') (y:ys')
      | x < y     = intersect' xs' (y:ys') -- x is smaller, discard x
      | x == y    = x : intersect' xs' ys' -- Common element, keep it
      | otherwise = intersect' (x:xs') ys' -- y is smaller, discard y

-- Checks if two sets are disjoint (have no elements in common).
isDisjointFrom :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CustomSet xs) (CustomSet ys) = disjoint' xs ys
  where
    -- Both lists xs and ys are sorted and unique.
    disjoint' :: (Ord a) => [a] -> [a] -> Bool
    disjoint' [] _ = True
    disjoint' _ [] = True
    disjoint' (x:xs') (y:ys')
      | x < y     = disjoint' xs' (y:ys') -- x is smaller, check rest of xs
      | x == y    = False                 -- Found common element
      | otherwise = disjoint' (x:xs') ys' -- y is smaller, check rest of ys

-- Checks if the first set is a subset of the second set.
isSubsetOf :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) (CustomSet ys) = subset' xs ys
  where
    -- Both lists xs and ys are sorted and unique.
    subset' :: (Ord a) => [a] -> [a] -> Bool
    subset' [] _ = True  -- Empty set is a subset of any set
    subset' _ [] = False -- Non-empty set cannot be a subset of empty set
    subset' (x:xs') (y:ys')
      | x < y     = False -- x is not in ys (since ys is sorted)
      | x == y    = subset' xs' ys' -- Found x in ys, check rest of xs against rest of ys
      | otherwise = subset' (x:xs') ys' -- y is smaller, check x against rest of ys

-- Checks if an element is a member of the set.
member :: (Ord a) => a -> CustomSet a -> Bool
member x (CustomSet xs) = member' x xs
  where
    -- List xs is sorted and unique.
    member' :: (Ord a) => a -> [a] -> Bool
    member' _ [] = False
    member' y (z:zs)
      | y < z     = False -- Element not found (since list is sorted)
      | y == z    = True  -- Element found
      | otherwise = member' y zs -- Keep searching

-- Checks if the set is empty. Our local 'null' shadows Prelude.null
null :: CustomSet a -> Bool
-- We can now use Prelude.null because it's no longer hidden.
null (CustomSet xs) = Prelude.null xs -- Use Prelude's null for lists

-- Returns the number of elements in the set.
size :: CustomSet a -> Int
size (CustomSet xs) = length xs -- Since list is unique, length is the size

-- Converts the set to a list of elements (in ascending order).
toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs -- Internal list is already sorted and unique

-- Returns the union of two sets (all unique elements from both).
union :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = CustomSet (union' xs ys)
  where
    -- Both lists xs and ys are sorted and unique.
    union' :: (Ord a) => [a] -> [a] -> [a]
    union' [] ys' = ys'
    union' xs' [] = xs'
    union' (x:xs') (y:ys')
      | x < y     = x : union' xs' (y:ys') -- Add x, advance in xs
      | x == y    = x : union' xs' ys'     -- Add one copy, advance in both
      | otherwise = y : union' (x:xs') ys' -- Add y, advance in ys
