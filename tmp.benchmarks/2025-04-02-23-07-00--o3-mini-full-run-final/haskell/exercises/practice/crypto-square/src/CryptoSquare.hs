module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)

encode :: String -> String
encode text
  | null normalized = ""
  | otherwise = encoded
  where
    normalized = [toLower ch | ch <- text, isAlphaNum ch]
    l = length normalized
    sqrtL = sqrt (fromIntegral l)
    r0 = floor sqrtL
    c0 = ceiling sqrtL
    r = if r0 * c0 < l then c0 else r0
    c = c0
    padLength = r * c
    missing = padLength - l
    padded = normalized ++ replicate (padLength - l) ' '
    rows = chunk c padded
    columns = [ [ row !! i | row <- rows ] | i <- [0 .. c - 1] ]
    columnsAdjusted = [ if i >= c - missing then col ++ " " else col | (i, col) <- zip [0..] columns ]
    encoded = unwords columnsAdjusted

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)
