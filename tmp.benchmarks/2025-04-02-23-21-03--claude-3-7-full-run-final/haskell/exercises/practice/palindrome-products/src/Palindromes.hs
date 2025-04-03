module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (sort, nub)

-- Check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

-- Find all palindrome products and their factors within a range
palindromeProducts :: Integer -> Integer -> [(Integer, [(Integer, Integer)])]
palindromeProducts minFactor maxFactor
  | minFactor > maxFactor = []
  | otherwise = 
      let factors = [(x, y) | x <- [minFactor..maxFactor], y <- [x..maxFactor]]
          products = [(x * y, (x, y)) | (x, y) <- factors]
          palindromes = filter (isPalindrome . fst) products
          grouped = groupByProduct palindromes
      in grouped
  where
    groupByProduct :: [(Integer, (Integer, Integer))] -> [(Integer, [(Integer, Integer)])]
    groupByProduct = foldr addToGroup []
    
    addToGroup :: (Integer, (Integer, Integer)) -> [(Integer, [(Integer, Integer)])] -> [(Integer, [(Integer, Integer)])]
    addToGroup (prod, pair) [] = [(prod, [pair])]
    addToGroup (prod, pair) ((p, pairs):rest)
      | prod == p = (p, pair:pairs) : rest
      | otherwise = (p, pairs) : addToGroup (prod, pair) rest

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | null products = Nothing
  | otherwise = Just (maximum products)
  where products = palindromeProducts minFactor maxFactor

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | null products = Nothing
  | otherwise = Just (minimum products)
  where products = palindromeProducts minFactor maxFactor
