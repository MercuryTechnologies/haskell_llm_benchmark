module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (nub, sort, groupBy)

-- Check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

-- Generate all possible products in range and their factors
allProducts :: Integer -> Integer -> [(Integer, (Integer, Integer))]
allProducts minFactor maxFactor =
  [(x * y, (x, y)) | x <- [minFactor..maxFactor], y <- [x..maxFactor]]

-- Find all palindromic products with their factors
palindromicProducts :: Integer -> Integer -> [(Integer, [(Integer, Integer)])]
palindromicProducts minFactor maxFactor =
  let products = allProducts minFactor maxFactor
      palindromes = filter (isPalindrome . fst) products
      grouped = groupByProduct palindromes
  in map (\xs@((p,_):_) -> (p, map snd xs)) grouped
  where
    groupByProduct = groupBy (\(a,_) (b,_) -> a == b)

-- Helper to group by product value
groupByProduct :: Eq a => [(a, b)] -> [[(a, b)]]
groupByProduct [] = []
groupByProduct (x:xs) = (x:ys) : groupByProduct zs
  where (ys, zs) = span (\(a,_) -> a == fst x) xs

-- Find largest palindrome product
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | null results = Nothing
  | otherwise = Just (last (sort results))
  where
    results = palindromicProducts minFactor maxFactor

-- Find smallest palindrome product
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | null results = Nothing
  | otherwise = Just (head (sort results))
  where
    results = palindromicProducts minFactor maxFactor
