module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs
  | n <= 0 = []  -- Return empty list for non-positive slice length
  | n > length xs = []
  | otherwise = map (map digitToInt) (windows n xs)
  where
    windows :: Int -> [a] -> [[a]]
    windows m ys
      | m > length ys = []
      | otherwise = take m ys : windows m (tail ys)
