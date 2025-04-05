module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n = n == sum (map (^ numDigits) digits)
  where
    digits = toDigits n
    numDigits = length digits

toDigits :: Integral a => a -> [a]
toDigits 0 = [0]
toDigits x
  | x < 0     = error "Negative numbers not supported"
  | otherwise = reverse (digitsHelper x)
  where
    digitsHelper 0 = []
    digitsHelper y = let (q, r) = y `divMod` 10 in r : digitsHelper q
