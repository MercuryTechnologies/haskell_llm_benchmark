module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n =
    let digitsOnly = filter (/= ' ') n
    in if length digitsOnly <= 1 || any (not . isDigit) digitsOnly
       then False
       else luhnCheck digitsOnly

luhnCheck :: String -> Bool
luhnCheck ds =
    let transformedDigits = zipWith transform [0..] (reverse ds)
        total = sum transformedDigits
    in total `mod` 10 == 0
  where
    transform i c =
      let d = digitToInt c
      in if odd i
         then let doubled = d * 2
              in if doubled > 9 then doubled - 9 else doubled
         else d
