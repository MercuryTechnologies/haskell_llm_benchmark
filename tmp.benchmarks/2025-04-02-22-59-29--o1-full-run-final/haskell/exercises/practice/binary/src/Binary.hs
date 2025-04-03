module Binary (toDecimal) where

import Data.List (foldl')

toDecimal :: String -> Int
toDecimal s
  | any (not . (`elem` "01")) s = 0
  | otherwise = foldl' (\acc c -> acc * 2 + digit c) 0 s
  where
    digit '1' = 1
    digit _   = 0
