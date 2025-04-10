module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n
  | n < 0     = False
  | otherwise = n == sum (map (^ numDigits) digits)
  where
    digits = toDigits n
    numDigits = length digits

    toDigits 0 = [0]
    toDigits x
      | x < 10    = [x]
      | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]
