module RotationalCipher (rotate) where

import Data.Char (isAlpha, isLower, ord, chr)

rotate :: Int -> String -> String
rotate shift = map (shiftChar shift)

shiftChar :: Int -> Char -> Char
shiftChar shift c
  | isAlpha c = shiftBase (ord base) c
  | otherwise = c
  where
    shiftAmount = shift `mod` 26
    base = if isLower c then 'a' else 'A'
    shiftBase baseChar char = chr $ baseChar + ((ord char - baseChar + shiftAmount) `mod` 26)
