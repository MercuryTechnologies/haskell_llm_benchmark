module RotationalCipher (rotate) where

import Data.Char (isAsciiLower, isAsciiUpper, ord, chr)

rotate :: Int -> String -> String
rotate key = map (rotateChar key)

rotateChar :: Int -> Char -> Char
rotateChar key c
  | isAsciiLower c = rotateLetter key 'a' c
  | isAsciiUpper c = rotateLetter key 'A' c
  | otherwise      = c

rotateLetter :: Int -> Char -> Char -> Char
rotateLetter key base c =
  let
    -- Calculate the position of the character relative to the base ('a' or 'A')
    offset = ord c - ord base
    -- Apply the rotation, wrapping around using modulo 26
    rotatedOffset = (offset + key) `mod` 26
    -- Calculate the ASCII value of the new character
    newOrd = ord base + rotatedOffset
  in
    -- Convert the ASCII value back to a character
    chr newOrd
