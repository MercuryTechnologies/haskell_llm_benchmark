module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n = n == sum [digit ^ power | digit <- digits]
  where
    digits = getDigits n
    power = length digits
    
    getDigits :: Integral a => a -> [a]
    getDigits 0 = []
    getDigits x = getDigits (x `div` 10) ++ [x `mod` 10]
    getDigits 0 = [0]  -- Special case for input 0
