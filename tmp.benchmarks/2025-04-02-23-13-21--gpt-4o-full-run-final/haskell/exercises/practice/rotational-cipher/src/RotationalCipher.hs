module RotationalCipher (rotate) where

import Data.Char (chr, ord, isAlpha, isUpper)

rotate :: Int -> String -> String
rotate key = map (rotateChar key)

rotateChar :: Int -> Char -> Char
rotateChar key c
  | isAlpha c = chr $ base + mod (ord c - base + key) 26
  | otherwise = c
  where
    base = if isUpper c then ord 'A' else ord 'a'
