module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (sortOn)

-- Helper function to check if a number is a palindrome.
isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

-- Given a range, generate all products and their factor pairs that are palindromes.
palindromeProducts :: Integer -> Integer -> [(Integer, (Integer, Integer))]
palindromeProducts minFactor maxFactor =
  [ (p, (i, j))
  | i <- [minFactor .. maxFactor]
  , j <- [minFactor .. maxFactor]
  , let p = i * j
  , isPalindrome p
  ]

-- Return the largest palindrome along with all of its factor pairs.
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
  let products = palindromeProducts minFactor maxFactor
  in if null products
       then Nothing
       else let sortedProducts = sortOn fst products
                (p, _) = last sortedProducts
                factors = [ pair | (prod, pair) <- products, prod == p ]
            in Just (p, factors)

-- Return the smallest palindrome along with all of its factor pairs.
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
  let products = palindromeProducts minFactor maxFactor
  in if null products
       then Nothing
       else let sortedProducts = sortOn fst products
                (p, _) = head sortedProducts
                factors = [ pair | (prod, pair) <- products, prod == p ]
            in Just (p, factors)
