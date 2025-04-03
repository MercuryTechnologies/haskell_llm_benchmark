module ArmstrongNumbers (armstrong) where

import Data.Char (digitToInt)

armstrong :: (Integral a, Show a) => a -> Bool
armstrong n = n == fromIntegral (sum (map (\d -> (digitToInt d) ^ p) str))
  where
    str = show n
    p = length str
