module Darts (score) where

score :: Float -> Float -> Int
score x y
  | radius <= 1  = 10  -- Inner circle (bullseye)
  | radius <= 5  = 5   -- Middle circle
  | radius <= 10 = 1   -- Outer circle
  | otherwise    = 0   -- Outside target
  where
    radius = sqrt (x^2 + y^2)  -- Calculate distance from center using Pythagorean theorem
