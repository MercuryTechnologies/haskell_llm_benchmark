module ArmstrongNumbers (armstrong) where

-- Helper function to calculate the number of digits in a non-negative integer.
-- For n > 0, it uses the logarithm base 10.
-- For n = 0, it returns 1, as 0 has one digit.
numberOfDigits :: Integral a => a -> Int
numberOfDigits n
  | n == 0    = 1
  | n > 0     = floor (logBase 10 (fromIntegral n)) + 1
  | otherwise = 0 -- Or handle error, but armstrong function checks for negatives

-- Helper function to calculate the sum of digits each raised to the power p.
sumDigitPowers :: Integral a => a -> Int -> a
sumDigitPowers n p = go n
  where
    go 0 = 0
    go x = (x `mod` 10) ^ p + go (x `div` 10)

-- | Determine whether a number is an Armstrong number.
-- An Armstrong number is a number that is the sum of its own digits
-- each raised to the power of the number of digits.
armstrong :: Integral a => a -> Bool
armstrong n
  | n < 0 = False -- Armstrong numbers are typically defined for non-negative integers
  | otherwise = n == sumDigitPowers n (numberOfDigits n)
