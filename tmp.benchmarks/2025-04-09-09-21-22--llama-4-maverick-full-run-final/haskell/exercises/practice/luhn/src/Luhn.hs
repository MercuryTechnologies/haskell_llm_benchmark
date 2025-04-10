module Luhn (isValid) where

isValid :: String -> Bool
isValid n = let cleaned = filter (`elem` ['0'..'9']) n
            in if length cleaned < 2
               then False
               else luhn (map (read . (:[])) cleaned :: [Int])

luhn :: [Int] -> Bool
luhn digits = sum (zipWith ($) (cycle [id, double]) (reverse digits)) `mod` 10 == 0
  where double x = if x * 2 > 9 then x * 2 - 9 else x * 2
