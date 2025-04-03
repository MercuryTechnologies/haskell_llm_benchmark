module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n = n == sum [d ^ length digits | d <- digits]
  where
    digits = toDigits (abs n)

toDigits :: Integral a => a -> [a]
toDigits 0 = [0]
toDigits n = go n []
  where
    go 0 acc = acc
    go m acc = go (m `div` 10) (m `mod` 10 : acc)
