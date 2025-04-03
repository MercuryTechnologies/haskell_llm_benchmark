module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.Char (isDigit)

isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

findPalindromeProducts :: Integer -> Integer -> [(Integer, (Integer, Integer))]
findPalindromeProducts minFactor maxFactor = do
  x <- [minFactor..maxFactor]
  y <- [x..maxFactor]
  let product = x * y
  if isPalindrome product
    then return (product, (x, y))
    else []

extractPalindrome :: (Integer, (Integer, Integer)) -> Integer
extractPalindrome (x, _) = x

groupFactors :: Integer -> [(Integer, (Integer, Integer))] -> [(Integer, Integer)]
groupFactors target products = map snd $ filter (\(x, _) -> x == target) products

constructResult :: Integer -> [(Integer, Integer)] -> Maybe (Integer, [(Integer, Integer)])
constructResult _ [] = Nothing
constructResult value factors = Just (value, factors)

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
  let
    products = findPalindromeProducts minFactor maxFactor
  in
    case products of
      [] -> Nothing
      _ -> let
              largest = maximum $ map extractPalindrome products
              factors = groupFactors largest products
            in
              constructResult largest factors

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
  let
    products = findPalindromeProducts minFactor maxFactor
  in
    case products of
      [] -> Nothing
      _ -> let
              smallest = minimum $ map extractPalindrome products
              factors = groupFactors smallest products
            in
              constructResult smallest factors
