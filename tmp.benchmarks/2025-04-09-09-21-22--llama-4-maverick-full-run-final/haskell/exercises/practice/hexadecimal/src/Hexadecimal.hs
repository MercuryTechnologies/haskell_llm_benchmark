module Hexadecimal (hexToInt) where

import Data.Char (isDigit, toLower, ord)

hexToInt :: String -> Maybe Int
hexToInt hexString
  | isValidHex hexString = Just (toDecimal hexString)
  | otherwise            = Nothing

isValidHex :: String -> Bool
isValidHex = all isHexDigit

isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || (toLower c >= 'a' && toLower c <= 'f')

toDecimal :: String -> Int
toDecimal = foldl (\acc c -> acc * 16 + digitToInt c) 0

digitToInt :: Char -> Int
digitToInt c
  | isDigit c = read [c]
  | otherwise = 10 + (ord (toLower c) - ord 'a')
