module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs =
  let digits = filter isDigit xs
      -- If the string has 11 digits and begins with '1', drop the '1'
      trimmed =
        if length digits == 11 && head digits == '1'
        then tail digits
        else digits
  in if validate trimmed
     then Just trimmed
     else Nothing

validate :: String -> Bool
validate s =
  length s == 10 &&
  s !! 0 `notElem` ['0', '1'] &&
  s !! 3 `notElem` ['0', '1']
