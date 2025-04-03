module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n
  | length digits <= 1 = False
  | otherwise = totalSum `mod` 10 == 0
  where
    digits = map digitToInt $ filter isDigit n
    totalSum = sum $ zipWith ($) (cycle [id, luhnDouble]) (reverse digits)

luhnDouble :: Int -> Int
luhnDouble x = let doubled = x * 2 in if doubled > 9 then doubled - 9 else doubled
