module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing  -- Return Nothing if sequences have different lengths
  | otherwise = Just (sum [1 | (x, y) <- zip xs ys, x /= y])  -- Count positions where characters differ
