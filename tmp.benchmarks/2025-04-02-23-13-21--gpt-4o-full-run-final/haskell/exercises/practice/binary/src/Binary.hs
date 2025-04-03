module Binary (toDecimal) where

import Data.Char (digitToInt)

toDecimal :: String -> Int
toDecimal xs
  | all (`elem` "01") xs = foldl (\acc x -> acc * 2 + digitToInt x) 0 xs
  | otherwise = error "Invalid input: Only binary digits (0 and 1) are allowed."
