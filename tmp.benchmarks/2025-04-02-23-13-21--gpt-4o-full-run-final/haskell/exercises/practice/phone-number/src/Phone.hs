module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs =
    let digits = filter isDigit xs
    in case digits of
        ('1':rest) | length rest == 10 -> validateNumber rest
        _ | length digits == 10 -> validateNumber digits
        _ -> Nothing

validateNumber :: String -> Maybe String
validateNumber num
    | length num == 10 && isValidAreaCode (take 3 num) && isValidExchangeCode (take 3 (drop 3 num)) = Just num
    | otherwise = Nothing

isValidAreaCode :: String -> Bool
isValidAreaCode (a:_) = a `elem` ['2'..'9']
isValidAreaCode _ = False

isValidExchangeCode :: String -> Bool
isValidExchangeCode (e:_) = e `elem` ['2'..'9']
isValidExchangeCode _ = False
