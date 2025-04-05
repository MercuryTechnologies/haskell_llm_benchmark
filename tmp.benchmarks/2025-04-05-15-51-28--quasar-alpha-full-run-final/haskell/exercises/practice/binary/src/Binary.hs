module Binary (toDecimal) where

import Data.Char (digitToInt)

toDecimal :: String -> Int
toDecimal xs
  | all (`elem` "01") xs = convert xs
  | otherwise = 0
  where
    convert = foldl (\acc x -> acc * 2 + digitToInt x) 0
