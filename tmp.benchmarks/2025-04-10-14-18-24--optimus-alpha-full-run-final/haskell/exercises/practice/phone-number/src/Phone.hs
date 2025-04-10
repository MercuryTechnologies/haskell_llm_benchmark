module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs =
    let digits = filter isDigit xs
        clean = case digits of
            ('1':rest) | length rest == 10 -> rest
            _ -> digits
    in if valid clean then Just clean else Nothing

valid :: String -> Bool
valid n =
    length n == 10 &&
    isValidAreaCode (take 3 n) &&
    isValidExchangeCode (take 3 (drop 3 n))

isValidAreaCode :: String -> Bool
isValidAreaCode (a:_:_) = a >= '2' && a <= '9'
isValidAreaCode _ = False

isValidExchangeCode :: String -> Bool
isValidExchangeCode (a:_:_) = a >= '2' && a <= '9'
isValidExchangeCode _ = False
