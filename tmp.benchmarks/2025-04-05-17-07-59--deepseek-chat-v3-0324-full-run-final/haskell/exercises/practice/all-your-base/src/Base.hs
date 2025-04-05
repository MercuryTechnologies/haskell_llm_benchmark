module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2 = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | null inputDigits = Right []
    | any (\d -> d < 0 || d >= inputBase) inputDigits = Left (InvalidDigit (head (filter (\d -> d < 0 || d >= inputBase) inputDigits)))
    | all (==0) inputDigits = Right [0]
    | otherwise = Right $ toDigits outputBase $ toDecimal inputBase inputDigits

toDecimal :: Integral a => a -> [a] -> a
toDecimal base digits = sum $ zipWith (*) (reverse digits) (map (base^) [0..length digits - 1])

toDigits :: Integral a => a -> a -> [a]
toDigits base n
    | n < base = [n]
    | otherwise = toDigits base (n `div` base) ++ [n `mod` base]
