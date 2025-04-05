module Luhn (isValid) where

import Data.Char (digitToInt, isDigit)

isValid :: String -> Bool
isValid input
  | length digits <= 1 = False
  | any (not . isDigit) stripped = False
  | otherwise = total `mod` 10 == 0
  where
    stripped = filter (/= ' ') input
    digits = map digitToInt stripped
    transformed = reverse $ zipWith transform [0..] (reverse digits)
    total = sum transformed

    transform :: Int -> Int -> Int
    transform idx d
      | idx `mod` 2 == 1 = let dbl = d * 2 in if dbl > 9 then dbl - 9 else dbl
      | otherwise = d
