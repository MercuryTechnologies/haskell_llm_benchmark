module Accumulate (accumulate) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate f xs = accumulateHelper f xs
  where
    accumulateHelper _ [] = []
    accumulateHelper f (y:ys) = f y : accumulateHelper f ys
