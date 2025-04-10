module BookStore (total, Book(..)) where

import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

data Book = First | Second | Third | Fourth | Fifth deriving (Eq, Ord, Enum, Bounded)

total :: [Book] -> Int
total books = total' (countBooks books)

countBooks :: [Book] -> [Int]
countBooks = map length . groupByBook . sortByBook
  where
    groupByBook :: [Book] -> [[Book]]
    groupByBook = group . sort
    sortByBook :: [Book] -> [Book]
    sortByBook = sort

total' :: [Int] -> Int
total' counts = total'' (sortOn Down counts)

total'' :: [Int] -> Int
total'' [] = 0
total'' counts = minCost
  where
    n = minimum counts
    discounted = map (\x -> x - n) counts
    minCost = n * discountedPrice (length counts) + total'' discounted

discountedPrice :: Int -> Int
discountedPrice n = round $ 800 * (1 - discount n)
  where
    discount :: Int -> Double
    discount 1 = 0
    discount 2 = 0.05
    discount 3 = 0.1
    discount 4 = 0.2
    discount 5 = 0.25
    discount _ = 0
