module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n
  | length cleanInput <= 1 = False  -- Strings of length 1 or less are not valid
  | not (all isDigit cleanInput) = False  -- All non-digit characters are disallowed
  | otherwise = sum processedDigits `mod` 10 == 0  -- Check if sum is divisible by 10
  where
    cleanInput = filter (/= ' ') n  -- Strip spaces
    digits = map digitToInt cleanInput
    indexedDigits = zip [0..] digits
    processedDigits = map processDigit indexedDigits
    
    -- Process each digit based on its position
    processDigit :: (Int, Int) -> Int
    processDigit (index, digit)
      | even (length digits - index - 1) = digit  -- Keep odd positions (from the right) unchanged
      | otherwise = doubleDigit digit  -- Double even positions (from the right)
    
    -- Double a digit and subtract 9 if the result is > 9
    doubleDigit :: Int -> Int
    doubleDigit x
      | doubled > 9 = doubled - 9
      | otherwise = doubled
      where doubled = x * 2
