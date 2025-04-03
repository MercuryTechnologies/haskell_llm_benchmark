module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (groupBy, sortOn)
import Data.Function (on)

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | null groupedPalindromes = Nothing
  | otherwise =
      let maxVal = maximum [p | (p, _) <- groupedPalindromes]
      in Just (maxVal, factorsFor maxVal)
  where
    isPalindrome :: Integer -> Bool
    isPalindrome n =
      let s = show n
      in s == reverse s

    -- collect all (product, (x,y)) for x,y in range if product is palindrome
    palProducts = [ (x*y, (x, y))
                  | x <- [minFactor..maxFactor]
                  , y <- [x..maxFactor]
                  , isPalindrome (x*y)
                  ]

    -- group them by product, so we can produce (product, [(x,y), (x',y'), ...])
    grouped = groupBy ((==) `on` fst) (sortOn fst palProducts)
    groupedPalindromes = map (\grp -> (fst (head grp), map snd grp)) grouped

    factorsFor prod = concat [fs | (p, fs) <- groupedPalindromes, p == prod]

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | null groupedPalindromes = Nothing
  | otherwise =
      let minVal = minimum [p | (p, _) <- groupedPalindromes]
      in Just (minVal, factorsFor minVal)
  where
    isPalindrome :: Integer -> Bool
    isPalindrome n =
      let s = show n
      in s == reverse s

    palProducts = [ (x*y, (x, y))
                  | x <- [minFactor..maxFactor]
                  , y <- [x..maxFactor]
                  , isPalindrome (x*y)
                  ]

    grouped = groupBy ((==) `on` fst) (sortOn fst palProducts)
    groupedPalindromes = map (\grp -> (fst (head grp), map snd grp)) grouped

    factorsFor prod = concat [fs | (p, fs) <- groupedPalindromes, p == prod]
