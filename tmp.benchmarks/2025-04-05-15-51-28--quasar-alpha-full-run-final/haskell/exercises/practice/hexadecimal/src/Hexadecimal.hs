module Hexadecimal (hexToInt) where

import Data.Char (toLower, isHexDigit, digitToInt)

hexToInt :: String -> Int
hexToInt s = 
    if all isHexDigit sLower
        then convert sLower
        else 0
  where
    sLower = map toLower s
    convert = foldl (\acc c -> acc * 16 + digitToInt c) 0
