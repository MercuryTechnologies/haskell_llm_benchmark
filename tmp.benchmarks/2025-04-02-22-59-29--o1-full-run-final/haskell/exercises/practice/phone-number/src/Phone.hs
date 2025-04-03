module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs =
    let digits = filter isDigit xs
    in case digits of
        _ | length digits == 10 ->
              validate digits
          | length digits == 11 && head digits == '1' ->
              validate (tail digits)
          | otherwise ->
              Nothing

validate :: String -> Maybe String
validate ds =
    let area = head ds
        exch = ds !! 3
    in if area `elem` ['2'..'9'] && exch `elem` ['2'..'9']
       then Just ds
       else Nothing
