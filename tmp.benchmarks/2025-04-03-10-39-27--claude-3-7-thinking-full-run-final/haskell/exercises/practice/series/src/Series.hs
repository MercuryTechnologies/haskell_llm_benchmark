module Series (slices) where

import Data.Char (digitToInt)
import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0     = []      -- Invalid length (negative)
  | n == 0    = [[]]    -- Zero length should return a list with an empty list
  | n > length xs = []  -- Requested length longer than string
  | otherwise = map (map digitToInt . take n) $ take (length xs - n + 1) $ tails xs
