module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n = n == sum (map (^ numDigits) digits)
  where
    digits = map (fromIntegral . (`mod` 10)) . takeWhile (> 0) $ iterate (`div` 10) n
    numDigits = length digits
