module Darts (score) where

score :: Float -> Float -> Int
score x y = 
  let distance = sqrt (x^2 + y^2)
  in case () of
       _ | distance > 10 -> 0
         | distance > 5  -> 1
         | distance > 1  -> 5
         | otherwise     -> 10
