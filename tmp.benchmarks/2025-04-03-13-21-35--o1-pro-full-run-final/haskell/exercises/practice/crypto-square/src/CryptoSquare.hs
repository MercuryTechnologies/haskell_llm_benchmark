module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

encode :: String -> String
encode text =
    let normalized = normalize text
        n = length normalized
        (r, c) = dimensions n
        padded = normalized ++ replicate (r*c - n) ' '
        rows = chunk c padded     -- Break into r rows, each of length c
        cols = transpose rows     -- Read down columns, left to right
    in unwords cols

-- Remove spaces and punctuation, lowercase letters/digits
normalize :: String -> String
normalize = map toLower . filter isAlphaNum

-- Compute rows (r) and columns (c)
dimensions :: Int -> (Int, Int)
dimensions n
  | n == 0    = (0, 0)
  | otherwise =
    let r0 = floor  (sqrt (fromIntegral n))
        c0 = ceiling(sqrt (fromIntegral n))
        r  = if r0 * c0 >= n then r0 else c0
    in (r, c0)

-- Utility to chunk the string into pieces of length c
chunk :: Int -> String -> [String]
chunk _ [] = []
chunk c xs = take c xs : chunk c (drop c xs)
