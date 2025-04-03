module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n =
  let cleaned = filter (/= ' ') n
  in if length cleaned <= 1 || any (not . isDigit) cleaned
     then False
     else
       let digits = map digitToInt cleaned
           -- Reverse the digits so that indexing starts from the rightmost digit
           reversedDigits = reverse digits
           processedDigits = zipWith (\i d -> if i `mod` 2 == 1
                                              then let doubled = d * 2
                                                   in if doubled > 9 then doubled - 9 else doubled
                                              else d)
                                      [0..]
                                      reversedDigits
           total = sum processedDigits
       in total `mod` 10 == 0
