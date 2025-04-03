module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs
  | n <= 0        = error "slice length must be greater than zero"
  | n > length xs = error "slice length is too big"
  | otherwise     = [map digitToInt (take n (drop i xs)) | i <- [0 .. length xs - n]]
