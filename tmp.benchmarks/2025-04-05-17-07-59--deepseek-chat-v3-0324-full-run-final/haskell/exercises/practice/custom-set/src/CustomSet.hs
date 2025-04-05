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
import qualified Prelude as P

newtype CustomSet a = CustomSet [a] deriving (Eq, Show)

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (filter (/= x) xs)

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) = CustomSet (filter (`notElem` ys) xs)

empty :: CustomSet a
empty = CustomSet []

fromList :: [a] -> CustomSet a
fromList xs = CustomSet (removeDuplicates xs)
  where removeDuplicates [] = []
        removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs) = if x `elem` xs then CustomSet xs else CustomSet (x:xs)

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) = CustomSet (filter (`elem` ys) xs)

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = null (intersection setA setB)

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) (CustomSet ys) = all (`elem` ys) xs

member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

null :: CustomSet a -> Bool
null (CustomSet xs) = P.null xs

size :: CustomSet a -> Int
size (CustomSet xs) = length xs

toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = fromList (xs ++ ys)
