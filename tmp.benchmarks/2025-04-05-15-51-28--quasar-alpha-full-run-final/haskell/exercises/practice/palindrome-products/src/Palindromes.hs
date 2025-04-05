module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (nub, sort)

-- Check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

-- Generate all factor pairs within the range
factorPairs :: Integer -> Integer -> [(Integer, Integer)]
factorPairs minF maxF = [(i, j) | i <- [minF..maxF], j <- [i..maxF]]

-- Find all palindrome products and their factor pairs
palindromeProducts :: Integer -> Integer -> [(Integer, (Integer, Integer))]
palindromeProducts minF maxF =
  [ (p, (i, j))
  | (i, j) <- factorPairs minF maxF
  , let p = i * j
  , isPalindrome p
  ]

-- Group factor pairs by palindrome product
groupByProduct :: [(Integer, (Integer, Integer))] -> [(Integer, [(Integer, Integer)])]
groupByProduct xs =
  let products = nub [p | (p, _) <- xs]
  in [ (p, [pair | (p', pair) <- xs, p' == p]) | p <- products ]

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minF maxF =
  let palProds = palindromeProducts minF maxF
      grouped = groupByProduct palProds
  in if null grouped
     then Nothing
     else let (p, pairs) = last (sort grouped)
          in Just (p, pairs)

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minF maxF =
  let palProds = palindromeProducts minF maxF
      grouped = groupByProduct palProds
  in if null grouped
     then Nothing
     else let (p, pairs) = head (sort grouped)
          in Just (p, pairs)
