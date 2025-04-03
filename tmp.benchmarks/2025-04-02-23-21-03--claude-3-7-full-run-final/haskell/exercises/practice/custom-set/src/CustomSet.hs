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
import Data.List (nub)

-- A CustomSet is represented as a list of elements with no duplicates
newtype CustomSet a = CustomSet { elements :: [a] }
  deriving Show

instance Eq a => Eq (CustomSet a) where
  -- Two sets are equal if they contain the same elements (regardless of order)
  set1 == set2 = all (`member` set2) (toList set1) && 
                 all (`member` set1) (toList set2)

-- Delete an element from a set
delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x set = CustomSet $ filter (/= x) (elements set)

-- Set difference: elements in setA that are not in setB
difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference setA setB = CustomSet $ filter (\x -> not $ member x setB) (elements setA)

-- Create an empty set
empty :: CustomSet a
empty = CustomSet []

-- Create a set from a list, removing duplicates
fromList :: Eq a => [a] -> CustomSet a
fromList xs = CustomSet $ nub xs

-- Insert an element into a set if it's not already present
insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x set
  | member x set = set
  | otherwise = CustomSet (x : elements set)

-- Set intersection: elements that are in both sets
intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection setA setB = CustomSet $ filter (`member` setB) (elements setA)

-- Check if two sets have no elements in common
isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = null (intersection setA setB)

-- Check if setA is a subset of setB
isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB = all (`member` setB) (elements setA)

-- Check if an element is in the set
member :: Eq a => a -> CustomSet a -> Bool
member x set = x `elem` elements set

-- Check if the set is empty
null :: Eq a => CustomSet a -> Bool
null set = elements set == []

-- Count the number of elements in the set
size :: CustomSet a -> Int
size set = length (elements set)

-- Convert a set to a list
toList :: CustomSet a -> [a]
toList = elements

-- Set union: elements that are in either set
union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union setA setB = fromList (elements setA ++ elements setB)
