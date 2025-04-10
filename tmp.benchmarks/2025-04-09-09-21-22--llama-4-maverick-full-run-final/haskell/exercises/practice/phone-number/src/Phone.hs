module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs = 
  let digits = filter isDigit xs
      cleaned = if length digits == 11 && head digits == '1' then tail digits else digits
  in if length cleaned == 10 && validNANP cleaned then Just cleaned else Nothing

validNANP :: String -> Bool
validNANP (a:b:c:d:e:f:_) = a >= '2' && a <= '9' && d >= '2' && d <= '9'
validNANP _ = False
