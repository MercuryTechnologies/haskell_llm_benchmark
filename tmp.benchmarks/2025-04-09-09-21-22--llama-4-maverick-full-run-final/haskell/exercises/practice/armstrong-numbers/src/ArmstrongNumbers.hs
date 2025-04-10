module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n = n == sumOfDigitsRaisedToPower n

sumOfDigitsRaisedToPower :: Integral a => a -> a
sumOfDigitsRaisedToPower n = sum [d ^ numDigits n | d <- digits n]

numDigits :: (Integral a, Show a) => a -> Int
numDigits n = length (show n)

digits :: Integral a => a -> [a]
digits 0 = []
digits n = n `mod` 10 : digits (n `div` 10)
