module Octal (readOct, showOct) where

import Data.Char (intToDigit)

readOct :: String -> Int
readOct = foldl (\acc c -> acc * 8 + digitToInt c) 0
  where
    digitToInt c
      | c >= '0' && c <= '7' = fromEnum c - fromEnum '0'
      | otherwise = 0

showOct :: Int -> String
showOct n
  | n <= 0 = "0"
  | otherwise = reverse $ go n
  where
    go 0 = []
    go x = intToDigit (x `mod` 8) : go (x `div` 8)
