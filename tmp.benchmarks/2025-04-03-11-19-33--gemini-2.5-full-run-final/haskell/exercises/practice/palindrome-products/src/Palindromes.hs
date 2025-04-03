module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (sort, nub)
import Data.Maybe (listToMaybe)

-- Helper function to check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = s == reverse s
  where s = show n

-- Helper function to find all palindrome products and their factors within the range
findPalindromeProducts :: Integer -> Integer -> [(Integer, (Integer, Integer))]
findPalindromeProducts minFactor maxFactor
  | minFactor > maxFactor = [] -- Handle invalid range
  | otherwise = filter (\(p, _) -> isPalindrome p) candidates
  where
    -- Generate all possible products and their factor pairs within the range
    candidates = [(i * j, (i, j)) | i <- [minFactor..maxFactor], j <- [minFactor..maxFactor]]

-- Find the largest palindrome product and its factors
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
  let palindromes = findPalindromeProducts minFactor maxFactor
  in case palindromes of
       [] -> Nothing -- No palindromes found
       ps -> let maxValue = maximum $ map fst ps
                 factors = nub . map sortFactorPair . map snd $ filter (\(p, _) -> p == maxValue) ps
             in Just (maxValue, factors)

-- Find the smallest palindrome product and its factors
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
  let palindromes = findPalindromeProducts minFactor maxFactor
  in case palindromes of
       [] -> Nothing -- No palindromes found
       ps -> let minValue = minimum $ map fst ps
                 factors = nub . map sortFactorPair . map snd $ filter (\(p, _) -> p == minValue) ps
             in Just (minValue, factors)

-- Helper to sort factor pairs to treat (a, b) and (b, a) as the same for uniqueness check
sortFactorPair :: (Integer, Integer) -> (Integer, Integer)
sortFactorPair (a, b) = (min a b, max a b)

