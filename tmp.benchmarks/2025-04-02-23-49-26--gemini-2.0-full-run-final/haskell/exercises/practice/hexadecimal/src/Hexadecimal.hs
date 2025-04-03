module Hexadecimal (hexToInt) where

hexToInt :: String -> Int
hexToInt hex = hexToInt' (reverse hex) 0 0
  where
    hexToInt' [] _ acc = acc
    hexToInt' (x:xs) power acc
      | val >= 0 = hexToInt' xs (power + 1) (acc + val * (16 ^ power))
      | otherwise = 0
      where val = hexDigitToInt x

hexDigitToInt :: Char -> Int
hexDigitToInt c
  | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
  | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
  | otherwise = -1
