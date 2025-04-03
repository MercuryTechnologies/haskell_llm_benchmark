module Luhn (isValid) where

isValid :: String -> Bool
isValid n
  | length cleaned <= 1 = False
  | otherwise = sumDigits `mod` 10 == 0
  where
    cleaned = filter (`elem` ['0'..'9']) (filter (/= ' ') n)
    sumDigits = sum $ zipWith ($) (cycle [id, double]) (reverse (map (\x -> read [x] :: Int) cleaned))

    double x = if x * 2 > 9 then x * 2 - 9 else x * 2
