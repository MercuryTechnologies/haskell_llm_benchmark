module Darts (score) where

score :: Float -> Float -> Int
score x y
  | distance <= 1 = 10
  | distance <= 5 = 5
  | distance <= 10 = 1
  | otherwise = 0
  where
    distance = sqrt (x^2 + y^2)
