module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0 = error "Slice length must not be negative"
  | n == 0 = replicate (length xs + 1) []
  | n > length xs = []
  | otherwise = map (map digitToInt . take n) (take (length xs - n + 1) (tails xs))
  where
    tails [] = []
    tails ys@(_:ys') = ys : tails ys'
