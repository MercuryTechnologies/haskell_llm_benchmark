module Binary (toDecimal) where

import Data.Char (digitToInt)

toDecimal :: String -> Int
toDecimal xs
  | all (`elem` "01") xs = foldl (\acc digit -> acc * 2 + digitToInt digit) 0 xs
  | otherwise = 0  -- Return 0 for invalid inputs instead of throwing an error
