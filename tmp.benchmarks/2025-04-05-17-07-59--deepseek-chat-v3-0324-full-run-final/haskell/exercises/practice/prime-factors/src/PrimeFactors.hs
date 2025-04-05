module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n 
    | n <= 1 = []
    | otherwise = factors n 2
    where
        factors 1 _ = []
        factors m d
            | m `mod` d == 0 = d : factors (m `div` d) d
            | otherwise = factors m (nextDivisor d)
        nextDivisor 2 = 3
        nextDivisor d = d + 2
