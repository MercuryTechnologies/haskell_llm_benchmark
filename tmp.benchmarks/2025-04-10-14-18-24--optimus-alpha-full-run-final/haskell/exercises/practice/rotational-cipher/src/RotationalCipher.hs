module RotationalCipher (rotate) where

import Data.Char (chr, ord, isLower, isUpper)

rotateChar :: Int -> Char -> Char
rotateChar n c
    | isLower c = chr $ base 'a' + ((ord c - base 'a' + n) `mod` 26)
    | isUpper c = chr $ base 'A' + ((ord c - base 'A' + n) `mod` 26)
    | otherwise = c
  where
    base ch = ord ch

rotate :: Int -> String -> String
rotate n = map (rotateChar n)
