module IsbnVerifier (isbn) where

import Data.Char (digitToInt, isDigit)

isbn :: String -> Bool
isbn code =
  let stripped = filter (/= '-') code
  in if length stripped /= 10
     then False
     else
       let first9 = take 9 stripped
           lastChar = stripped !! 9
       in if all isDigit first9
             && (isDigit lastChar || lastChar == 'X')
          then
            let digits = map digitToInt first9
                lastValue = if lastChar == 'X' then 10 else digitToInt lastChar
                weightedSum = sum $ zipWith (*) (digits ++ [lastValue]) [10,9..1]
            in weightedSum `mod` 11 == 0
          else False
