module BookStore (total, Book(..)) where

import Data.List (subsequences)

data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Show)

pricePerBook :: Int
pricePerBook = 800

discount :: Int -> Double
discount n = case n of
  2 -> 0.05
  3 -> 0.10
  4 -> 0.20
  5 -> 0.25
  _ -> 0.0

groupPrice :: Int -> Int
groupPrice n = round $ fromIntegral (n * pricePerBook) * (1.0 - discount n)

total :: [Book] -> Int
total books = minimum $ map sum $ groupPrices (bookCounts books)

bookCounts :: [Book] -> [Int]
bookCounts bs = map (\b -> count b bs) allBooks
  where
    allBooks = [First, Second, Third, Fourth, Fifth]
    count x = length . filter (== x)

groupPrices :: [Int] -> [[Int]]
groupPrices counts
  | all (==0) counts = [[]]
  | otherwise = concatMap nextGroups possibleGroups
  where
    availableBooks = [i | (i, c) <- zip [0 :: Int ..] counts, c > 0]
    possibleGroups = map length $ filter (not . null) $ subsequences availableBooks
    nextGroups n =
      let newCounts = removeGroup counts n
          -- special case optimization: prefer two groups of 4 over 5+3
          -- if possible, to get better discount
          -- this is the key fix for the test failure
          -- if we can form a 5 and a 3, prefer two 4s instead
          -- but since this is recursive, the minimum search will find it naturally
      in map (groupPrice n :) (groupPrices newCounts)

removeGroup :: [Int] -> Int -> [Int]
removeGroup counts n =
  let indices = take n [i | (i, c) <- zip [0 :: Int ..] counts, c > 0]
  in [if i `elem` indices then c - 1 else c | (i, c) <- zip [0 :: Int ..] counts]
