module Transpose (transpose) where

transpose :: [String] -> [String]
transpose [] = []
transpose xs = [padLeft (map (!! i) xs) | i <- [0..maxLength - 1]]
  where
    maxLength = maximum (map length xs)
    padLeft col = let len = length col in replicate (maxLength - len) ' ' ++ col
