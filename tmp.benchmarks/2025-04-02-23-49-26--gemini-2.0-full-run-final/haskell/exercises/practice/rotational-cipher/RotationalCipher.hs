module RotationalCipher (rotate) where

import Data.Char (ord, chr, isAsciiLower, isAsciiUpper)

rotate :: Int -> String -> String
rotate key str = map (rotateChar key) str

rotateChar :: Int -> Char -> Char
rotateChar key char
  | isAsciiLower char = chr $ (ord 'a' + (ord char - ord 'a' + key) `mod` 26)
  | isAsciiUpper char = chr $ (ord 'A' + (ord char - ord 'A' + key) `mod` 26)
  | otherwise = char
