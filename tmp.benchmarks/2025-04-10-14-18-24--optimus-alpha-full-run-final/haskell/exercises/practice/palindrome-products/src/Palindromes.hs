module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (nub, sort, groupBy)
import Data.Maybe (listToMaybe)
import Data.Function (on)

-- Check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

-- Generate all factor pairs (a, b) with minFactor <= a <= b <= maxFactor
factorPairs :: Integer -> Integer -> [(Integer, Integer)]
factorPairs minF maxF = [(a, b) | a <- [minF..maxF], b <- [a..maxF]]

-- For a given range, get all palindrome products and their factor pairs
palindromeProducts :: Integer -> Integer -> [(Integer, (Integer, Integer))]
palindromeProducts minF maxF =
  [ (p, (a, b))
  | (a, b) <- factorPairs minF maxF
  , let p = a * b
  , isPalindrome p
  ]

-- Group palindrome products by their value, collecting all factor pairs
groupPalindromes :: [(Integer, (Integer, Integer))] -> [(Integer, [(Integer, Integer)])]
groupPalindromes xs =
  map (\grp -> (fst (head grp), map snd grp)) $
    groupBy ((==) `on` fst) $
    sort xs

-- Find the smallest palindrome and its factor pairs
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minF maxF =
  let palProds = palindromeProducts minF maxF
      grouped = groupPalindromes palProds
  in case grouped of
       [] -> Nothing
       _  -> Just $ head grouped

-- Find the largest palindrome and its factor pairs
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minF maxF =
  let palProds = palindromeProducts minF maxF
      grouped = groupPalindromes palProds
  in case grouped of
       [] -> Nothing
       _  -> Just $ last grouped
