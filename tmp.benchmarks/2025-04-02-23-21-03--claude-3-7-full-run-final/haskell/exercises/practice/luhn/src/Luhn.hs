module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n
  | length cleanInput <= 1 = False
  | not (all isDigit cleanInput) = False
  | otherwise = sumDigits `mod` 10 == 0
  where
    cleanInput = filter (/= ' ') n
    digits = map digitToInt cleanInput
    
    -- Double every second digit starting from the right
    transformedDigits = reverse $ zipWith transform [1..] (reverse digits)
    
    transform :: Int -> Int -> Int
    transform i d
      | i `mod` 2 == 0 = let doubled = d * 2 in if doubled > 9 then doubled - 9 else doubled
      | otherwise = d
    
    sumDigits = sum transformedDigits
