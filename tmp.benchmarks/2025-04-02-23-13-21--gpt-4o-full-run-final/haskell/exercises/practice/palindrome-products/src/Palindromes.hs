module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (nub)

-- Helper function to check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

-- Function to find the largest palindrome product
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor = 
    let products = [(x * y, (x, y)) | x <- [minFactor..maxFactor], y <- [x..maxFactor]]
        palindromes = filter (isPalindrome . fst) products
        maxPalindrome = if null palindromes then Nothing else Just (maximum (map fst palindromes))
    in case maxPalindrome of
        Nothing -> Nothing
        Just p -> Just (p, nub [factors | (product, factors) <- palindromes, product == p])

-- Function to find the smallest palindrome product
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor = 
    let products = [(x * y, (x, y)) | x <- [minFactor..maxFactor], y <- [x..maxFactor]]
        palindromes = filter (isPalindrome . fst) products
        minPalindrome = if null palindromes then Nothing else Just (minimum (map fst palindromes))
    in case minPalindrome of
        Nothing -> Nothing
        Just p -> Just (p, nub [factors | (product, factors) <- palindromes, product == p])
