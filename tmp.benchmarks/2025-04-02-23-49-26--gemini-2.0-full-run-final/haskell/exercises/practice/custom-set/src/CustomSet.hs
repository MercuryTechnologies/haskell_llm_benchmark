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

import Prelude hiding ()

data CustomSet a = CustomSet [a] deriving (Eq, Show)

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet $ filter (/= x) xs

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet setA) (CustomSet setB) = CustomSet $ filter (`notElem` setB) setA

empty :: CustomSet a
empty = CustomSet []

fromList :: Eq a => [a] -> CustomSet a
fromList xs = CustomSet $ nub xs

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs) = CustomSet $ if x `elem` xs then xs else x : xs

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet setA) (CustomSet setB) = CustomSet $ filter (`elem` setB) setA

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = null $ intersection setA setB

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet setA) (CustomSet setB) = all (`elem` setB) setA

member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

null :: CustomSet a -> Bool
null (CustomSet xs) = Prelude.null xs

size :: CustomSet a -> Int
size (CustomSet xs) = length xs

toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet setA) (CustomSet setB) = CustomSet $ nub (setA ++ setB)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)
