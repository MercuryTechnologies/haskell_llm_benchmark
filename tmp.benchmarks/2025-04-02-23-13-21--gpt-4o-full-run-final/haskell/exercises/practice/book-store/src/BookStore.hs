module BookStore (total, Book(..)) where

import Data.List (nub, delete, sort)

data Book = First | Second | Third | Fourth | Fifth deriving (Eq, Show)

bookPrice :: Int
bookPrice = 800 -- price in cents

discounts :: [(Int, Double)]
discounts = [(5, 0.25), (4, 0.20), (3, 0.10), (2, 0.05), (1, 0.0)]

total :: [Book] -> Int
total basket = minimum $ map priceForCombination (optimalCombinations basket)

priceForCombination :: [[Book]] -> Int
priceForCombination = sum . map priceForSet

priceForSet :: [Book] -> Int
priceForSet set = round $ fromIntegral (length set * bookPrice) * (1 - discount)
  where
    discount = maybe 0 id (lookup (length set) discounts)

optimalCombinations :: [Book] -> [[[Book]]]
optimalCombinations books = map (map (map fst)) $ groupBooks (map (\b -> (b, 1)) books)

groupBooks :: [(Book, Int)] -> [[[(Book, Int)]]]
groupBooks [] = [[]]
groupBooks books = [set : rest | set <- uniqueSets books, rest <- groupBooks (removeSet set books)]

uniqueSets :: [(Book, Int)] -> [[(Book, Int)]]
uniqueSets books = filter (not . null) $ map (`takeUnique` books) [5, 4, 3, 2, 1]

takeUnique :: Int -> [(Book, Int)] -> [(Book, Int)]
takeUnique n books = take n $ filter (\(_, count) -> count > 0) $ nub books

removeSet :: [(Book, Int)] -> [(Book, Int)] -> [(Book, Int)]
removeSet [] books = books
removeSet ((b, _):xs) books = removeSet xs (map (\(book, count) -> if book == b then (book, count - 1) else (book, count)) books)
