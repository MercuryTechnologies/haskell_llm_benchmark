module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (sortOn, groupBy)
import Data.Function (on)

-- | Checks if a number is a palindrome.
isPalindrome :: Integer -> Bool
isPalindrome n = s == reverse s
  where s = show n

-- | Finds all palindrome products within the given range and groups them by product value.
-- Returns a list of groups, where each group contains tuples of (product, (factor1, factor2)).
-- The groups are sorted by the product value.
findAndGroupPalindromes :: Integer -> Integer -> [[(Integer, (Integer, Integer))]]
findAndGroupPalindromes minFactor maxFactor
  -- If the range is invalid, return empty list.
  | minFactor > maxFactor = []
  | otherwise =
      let -- Generate all possible products and their factor pairs within the range.
          products = [(i * j, (i, j)) | i <- [minFactor..maxFactor], j <- [minFactor..maxFactor]]
          -- Filter to keep only those products that are palindromes.
          palindromes = filter (\(p, _) -> isPalindrome p) products
      in -- If no palindromes are found, return empty list.
         if null palindromes
         then []
         -- Otherwise, sort by the palindrome value and group by it.
         else groupBy ((==) `on` fst) $ sortOn fst palindromes

-- | Formats a group of (product, factors) tuples into the required output format.
formatResult :: [(Integer, (Integer, Integer))] -> (Integer, [(Integer, Integer)])
formatResult group = (fst (head group), map snd group)

-- | Finds the largest palindrome product within the given range.
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
    case findAndGroupPalindromes minFactor maxFactor of
        -- If no palindrome groups were found, return Nothing.
        [] -> Nothing
        -- Otherwise, take the last group (largest palindrome) and format it.
        groups -> Just $ formatResult (last groups)

-- | Finds the smallest palindrome product within the given range.
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
    case findAndGroupPalindromes minFactor maxFactor of
        -- If no palindrome groups were found, return Nothing.
        [] -> Nothing
        -- Otherwise, take the first group (smallest palindrome) and format it.
        groups -> Just $ formatResult (head groups)
