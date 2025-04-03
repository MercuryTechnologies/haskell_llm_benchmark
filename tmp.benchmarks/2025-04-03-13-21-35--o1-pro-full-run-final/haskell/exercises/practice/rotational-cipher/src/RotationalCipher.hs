module RotationalCipher (rotate) where

import Data.Char (chr, ord)

rotate :: Int -> String -> String
rotate n = map (shiftChar n)

shiftChar :: Int -> Char -> Char
shiftChar shift c
    | 'a' <= c && c <= 'z' =
        let offset = ord c - ord 'a'
            newOffset = (offset + shift) `mod` 26
        in chr (ord 'a' + newOffset)
    | 'A' <= c && c <= 'Z' =
        let offset = ord c - ord 'A'
            newOffset = (offset + shift) `mod` 26
        in chr (ord 'A' + newOffset)
    | otherwise = c
