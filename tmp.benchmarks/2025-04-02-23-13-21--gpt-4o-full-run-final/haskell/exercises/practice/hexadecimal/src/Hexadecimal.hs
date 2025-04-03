module Hexadecimal (hexToInt) where

import Data.Char (digitToInt, toLower)

hexToInt :: String -> Int
hexToInt hexStr
  | all isValidHexChar hexStr = foldl (\acc x -> acc * 16 + x) 0 (map hexCharToInt hexStr)
  | otherwise = 0
  where
    isValidHexChar c = c `elem` "0123456789abcdefABCDEF"
    hexCharToInt c
      | c `elem` "0123456789" = digitToInt c
      | c `elem` "abcdef" = 10 + (digitToInt c - digitToInt 'a')
      | c `elem` "ABCDEF" = 10 + (digitToInt (toLower c) - digitToInt 'a')
      | otherwise = 0
