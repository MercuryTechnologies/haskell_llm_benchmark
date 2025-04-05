module RotationalCipher (rotate) where

import Data.Char (chr, ord, isLower, isUpper)

rotate :: Int -> String -> String
rotate key = map (rotateChar (key `mod` 26))

rotateChar :: Int -> Char -> Char
rotateChar key c
  | isLower c = shift 'a' c
  | isUpper c = shift 'A' c
  | otherwise = c
  where
    shift base ch = chr $ ord base + ((ord ch - ord base + key) `mod` 26)
