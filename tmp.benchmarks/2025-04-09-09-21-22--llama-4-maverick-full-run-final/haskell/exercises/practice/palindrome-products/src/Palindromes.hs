module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (sort)

isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

products :: Integer -> Integer -> [(Integer, (Integer, Integer))]
products minFactor maxFactor = 
    [(x*y, (x, y)) | x <- [minFactor..maxFactor], y <- [x..maxFactor]]

palindromeProducts :: Integer -> Integer -> [(Integer, (Integer, Integer))]
palindromeProducts minFactor maxFactor = 
    filter (isPalindrome . fst) (products minFactor maxFactor)

groupFactors :: [(Integer, (Integer, Integer))] -> [(Integer, [(Integer, Integer)])]
groupFactors = 
    map (\xs -> (fst (head xs), map snd xs)) . 
    groupBy (\x y -> fst x == fst y) . 
    sort

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor = 
    case groupFactors (palindromeProducts minFactor maxFactor) of
        [] -> Nothing
        (x:_) -> Just x

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor = 
    case groupFactors (palindromeProducts minFactor maxFactor) of
        [] -> Nothing
        xs -> Just (last xs)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy eq (x:xs) = (x:ys) : groupBy eq zs
  where (ys, zs) = span (eq x) xs
