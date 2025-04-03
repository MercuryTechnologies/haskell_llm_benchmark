module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)
import Data.List (reverse)

isValid :: String -> Bool
isValid number =
  let stripped = filter isDigit number
      len = length stripped
  in if len <= 1
     then False
     else
       let digits = map digitToInt (reverse stripped)
           doubled = zipWith (\i d -> if i `mod` 2 == 1 then doubleDigit d else d) [0..] digits
           summed = sum doubled
       in summed `mod` 10 == 0

doubleDigit :: Int -> Int
doubleDigit d =
  let doubled = d * 2
  in if doubled > 9 then doubled - 9 else doubled
