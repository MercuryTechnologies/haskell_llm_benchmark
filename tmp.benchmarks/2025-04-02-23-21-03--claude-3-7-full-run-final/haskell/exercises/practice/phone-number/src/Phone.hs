module Phone (number) where

import Data.Char (isDigit, isNumber)

number :: String -> Maybe String
number xs = 
  case cleanedNumber of
    num | length num == 10 && isValidNANP num -> Just num
        | length num == 11 && head num == '1' && isValidNANP (tail num) -> Just (tail num)
        | otherwise -> Nothing
  where
    cleanedNumber = filter isDigit xs
    isValidNANP num = 
      let areaCode = take 3 num
          exchangeCode = take 3 (drop 3 num)
      in head areaCode >= '2' && head areaCode <= '9' && 
         head exchangeCode >= '2' && head exchangeCode <= '9'
