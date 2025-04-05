module Darts (score) where

score :: Float -> Float -> Int
score x y
  | distSquared > 100 = 0
  | distSquared > 25  = 1
  | distSquared > 1   = 5
  | otherwise         = 10
  where
    distSquared = x * x + y * y
