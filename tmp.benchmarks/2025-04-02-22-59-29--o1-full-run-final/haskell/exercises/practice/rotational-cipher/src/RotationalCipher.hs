module RotationalCipher (rotate) where

import Data.Char (isLower, isUpper, ord, chr)

rotate :: Int -> String -> String
rotate key = map (rotateChar (key `mod` 26))
  where
    rotateChar :: Int -> Char -> Char
    rotateChar k c
      | isUpper c =
          let base = ord 'A'
              offset = ord c - base
              rotated = (offset + k) `mod` 26
          in chr (base + rotated)
      | isLower c =
          let base = ord 'a'
              offset = ord c - base
              rotated = (offset + k) `mod` 26
          in chr (base + rotated)
      | otherwise = c
