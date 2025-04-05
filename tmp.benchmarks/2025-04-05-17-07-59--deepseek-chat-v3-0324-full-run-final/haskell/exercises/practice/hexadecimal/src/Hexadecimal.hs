module Hexadecimal (hexToInt) where

import Data.Char (toLower, isHexDigit)
import Data.List (foldl')

hexToInt :: String -> Int
hexToInt [] = error "Empty hex string"
hexToInt s 
    | all isHexDigit s = foldl' (\acc c -> 16 * acc + digitToInt c) 0 (map toLower s)
    | otherwise = error $ "Invalid hex digit in: " ++ s
    where
        digitToInt c
            | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
            | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
            | otherwise = error $ "Invalid hex digit: " ++ [c]
