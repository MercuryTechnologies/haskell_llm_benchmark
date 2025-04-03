module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs = 
  let digits = filter isDigit xs
      digits' = if length digits > 10 && head digits == '1'
                then tail digits
                else digits
  in if isValidNANP digits'
     then Just digits'
     else Nothing

isValidNANP :: String -> Bool
isValidNANP digits = 
  length digits == 10 &&
  digits !! 0 >= '2' && digits !! 0 <= '9' &&
  digits !! 3 >= '2' && digits !! 3 <= '9'
