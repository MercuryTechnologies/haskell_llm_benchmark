module ArmstrongNumbers (armstrong) where

armstrong :: (Integral a, Read a, Show a) => a -> Bool
armstrong n = n == sum (map (^ digitCount) digits)
  where
    digits = map (read . (:[])) (show n)
    digitCount = length digits
