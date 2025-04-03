module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor = 
    let products = [(x * y, (x, y)) | x <- [minFactor..maxFactor], y <- [x..maxFactor]]
        palindromes = filter (isPalindrome . fst) products
        largest = maximumBy (comparing fst) palindromes
    in Just (fst largest, map snd (filter ((== fst largest) . fst) palindromes))

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor = 
    let products = [(x * y, (x, y)) | x <- [minFactor..maxFactor], y <- [x..maxFactor]]
        palindromes = filter (isPalindrome . fst) products
        smallest = minimumBy (comparing fst) palindromes
    in Just (fst smallest, map snd (filter ((== fst smallest) . fst) palindromes))

isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)
