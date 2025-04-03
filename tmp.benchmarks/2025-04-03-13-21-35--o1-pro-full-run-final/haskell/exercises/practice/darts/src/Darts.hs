module Darts (score) where

score :: Float -> Float -> Int
score x y =
  let r = sqrt (x * x + y * y)
  in if r <= 1
     then 10
     else if r <= 5
     then 5
     else if r <= 10
     then 1
     else 0
