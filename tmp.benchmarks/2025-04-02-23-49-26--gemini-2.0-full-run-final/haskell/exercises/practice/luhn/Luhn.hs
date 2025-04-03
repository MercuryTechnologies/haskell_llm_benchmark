module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid number =
  let stripped = filter isDigit number
      len = length stripped
  in if len <= 1
     then False
     else
       let digits = map digitToInt stripped
           doubled = zipWith (\i d -> if i `mod` 2 == 0 then doubleDigit d else d) [len-1,len-2..0] digits
           summed = sum doubled
       in summed `mod` 10 == 0

doubleDigit :: Int -> Int
doubleDigit d =
  let doubled = d * 2
  in if doubled > 9 then doubled - 9 else doubled
