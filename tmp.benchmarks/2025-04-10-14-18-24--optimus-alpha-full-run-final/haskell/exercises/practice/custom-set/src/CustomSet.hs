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

-- | CustomSet is a wrapper around a list with unique elements.
newtype CustomSet a = CustomSet [a]

-- Eq instance: sets are equal if they have the same elements, regardless of order or duplicates in the list
instance (Eq a) => Eq (CustomSet a) where
  CustomSet xs == CustomSet ys = L.null (xs L.\\ ys) && L.null (ys L.\\ xs)

instance (Show a) => Show (CustomSet a) where
  show (CustomSet xs) = "fromList " ++ show xs

-- | Remove all occurrences of an element from the set.
delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (filter (/= x) xs)

-- | Elements in setA but not in setB.
difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) = CustomSet (filter (`notElem` ys) xs)

-- | The empty set.
empty :: CustomSet a
empty = CustomSet []

-- | Create a set from a list, removing duplicates.
fromList :: Eq a => [a] -> CustomSet a
fromList = CustomSet . L.nub

-- | Insert an element into the set.
insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs)
  | x `elem` xs = CustomSet xs
  | otherwise   = CustomSet (x:xs)

-- | Elements common to both sets.
intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) = CustomSet (filter (`elem` ys) xs)

-- | True if the sets have no elements in common.
isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CustomSet xs) (CustomSet ys) = all (`notElem` ys) xs

-- | True if all elements of setA are in setB.
isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) (CustomSet ys) = all (`elem` ys) xs

-- | True if the element is in the set.
member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

-- | True if the set is empty.
null :: CustomSet a -> Bool
null (CustomSet xs) = L.null xs

-- | The number of elements in the set.
size :: CustomSet a -> Int
size (CustomSet xs) = length (L.nub xs)

-- | Convert the set to a list.
toList :: CustomSet a -> [a]
toList (CustomSet xs) = L.nub xs

-- | The union of two sets.
union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = fromList (xs ++ ys)
