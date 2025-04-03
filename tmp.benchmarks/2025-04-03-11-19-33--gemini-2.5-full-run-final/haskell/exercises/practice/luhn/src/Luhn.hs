module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

-- | Doubles a digit according to Luhn rules.
-- If doubling results in a number > 9, subtract 9.
luhnDouble :: Int -> Int
luhnDouble d = let doubled = d * 2
               in if doubled > 9 then doubled - 9 else doubled

-- | Applies the Luhn transformation to a list of digits (already reversed).
-- Doubles every second digit (at odd indices).
luhnTransform :: [Int] -> [Int]
luhnTransform digits = zipWith ($) (cycle [id, luhnDouble]) digits

isValid :: String -> Bool
isValid n =
  let cleaned = filter (/= ' ') n
  in if length cleaned <= 1 || not (all isDigit cleaned)
     then False
     else let digits = map digitToInt cleaned
              reversedDigits = reverse digits
              transformedDigits = luhnTransform reversedDigits
              totalSum = sum transformedDigits
          in totalSum `mod` 10 == 0
