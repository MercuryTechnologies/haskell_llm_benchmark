module RotationalCipher (rotate) where

import Data.Char (isLower, isUpper, ord, chr)

rotate :: Int -> String -> String
rotate key = map (rotateChar key)

-- Helper function to rotate a single character
rotateChar :: Int -> Char -> Char
rotateChar key c
  | isLower c = shiftChar 'a' key c
  | isUpper c = shiftChar 'A' key c
  | otherwise = c  -- Non-alphabetic characters remain unchanged

-- Shift a character by the given key, using the base character as a reference point
shiftChar :: Char -> Int -> Char -> Char
shiftChar base key c = chr $ ord base + (ord c - ord base + normalizedKey) `mod` 26
  where normalizedKey = key `mod` 26  -- Handle keys with modulo arithmetic
