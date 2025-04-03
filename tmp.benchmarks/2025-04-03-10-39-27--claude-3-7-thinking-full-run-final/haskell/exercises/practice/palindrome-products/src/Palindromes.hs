module Palindromes (largestPalindrome, smallestPalindrome) where

-- Check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

-- Generate all palindrome products and their factor pairs within a range
palindromeProducts :: Integer -> Integer -> [(Integer, (Integer, Integer))]
palindromeProducts minFactor maxFactor = 
    [(x * y, (x, y)) | x <- [minFactor..maxFactor], 
                        y <- [x..maxFactor], 
                        isPalindrome (x * y)]

-- Find the largest palindrome product and its factors
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
    | minFactor > maxFactor = Nothing
    | otherwise = 
        let pairs = palindromeProducts minFactor maxFactor
        in if null pairs
           then Nothing
           else let largest = maximum (map fst pairs)
                    largestPairs = [y | (x, y) <- pairs, x == largest]
                in Just (largest, largestPairs)

-- Find the smallest palindrome product and its factors
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
    | minFactor > maxFactor = Nothing
    | otherwise = 
        let pairs = palindromeProducts minFactor maxFactor
        in if null pairs
           then Nothing
           else let smallest = minimum (map fst pairs)
                    smallestPairs = [y | (x, y) <- pairs, x == smallest]
                in Just (smallest, smallestPairs)
