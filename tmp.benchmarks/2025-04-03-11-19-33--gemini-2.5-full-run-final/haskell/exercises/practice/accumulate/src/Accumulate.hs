module Accumulate (accumulate) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate _ [] = [] -- Base case: If the list is empty, return an empty list.
accumulate f (x:xs) = f x : accumulate f xs -- Recursive step: Apply f to the head (x),
                                           -- and prepend it to the result of accumulating over the tail (xs).
