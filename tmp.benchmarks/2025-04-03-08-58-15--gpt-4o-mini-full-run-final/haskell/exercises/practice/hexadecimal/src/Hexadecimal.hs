module Hexadecimal (hexToInt) where

import Data.Char (digitToInt, isHexDigit)

hexToInt :: String -> Either String Int
hexToInt hexString
    | all isHexDigit hexString = Right (convert hexString (length hexString - 1))
    | otherwise = Left "Invalid hexadecimal string"

convert :: String -> Int -> Int
convert [] _ = 0
convert (x:xs) exp = digitToInt x * (16 ^ exp) + convert xs (exp - 1)
