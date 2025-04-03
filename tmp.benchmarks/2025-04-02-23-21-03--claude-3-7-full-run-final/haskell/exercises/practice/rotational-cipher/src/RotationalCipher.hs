module RotationalCipher (rotate) where

import Data.Char (isLower, isUpper, ord, chr)

rotate :: Int -> String -> String
rotate key = map (rotateChar key)

rotateChar :: Int -> Char -> Char
rotateChar key c
  | isLower c = shiftChar 'a' key c
  | isUpper c = shiftChar 'A' key c
  | otherwise = c  -- Keep non-alphabetic characters unchanged

shiftChar :: Char -> Int -> Char -> Char
shiftChar base key c = chr $ ((ord c - ord base + key) `mod` 26) + ord base
