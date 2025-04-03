module Darts (score) where

score :: Float -> Float -> Int
score x y
  | distSq > 100 = 0 -- Outside the target
  | distSq > 25  = 1 -- Outer circle
  | distSq > 1   = 5 -- Middle circle
  | otherwise    = 10 -- Inner circle (Bullseye)
  where
    distSq = x*x + y*y
