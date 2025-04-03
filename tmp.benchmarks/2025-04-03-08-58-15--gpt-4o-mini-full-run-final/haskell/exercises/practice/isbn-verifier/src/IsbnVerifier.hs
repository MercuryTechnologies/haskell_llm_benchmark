module IsbnVerifier (isbn) where

import Data.Char (isDigit, toUpper)

isbn :: String -> Bool
isbn str = 
    let cleanStr = filter (/= '-') str
        digits = map toUpper cleanStr
        checkDigit = last digits
        numberPart = init digits
        total = sum [digitToInt (numberPart !! i) * (10 - i) | i <- [0..8]]
        totalWithCheck = total + (if checkDigit == 'X' then 10 else digitToInt checkDigit)
    in length cleanStr == 10 && totalWithCheck `mod` 11 == 0

digitToInt :: Char -> Int
digitToInt c
    | isDigit c = fromEnum c - fromEnum '0'
    | c == 'X' = 10
    | otherwise = 0  -- Return 0 for invalid characters instead of throwing an error
