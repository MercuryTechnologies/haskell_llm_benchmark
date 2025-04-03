module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase <= 1 = Left InvalidInputBase
    | outputBase <= 1 = Left InvalidOutputBase
    | any (\d -> d < 0 || d >= inputBase) inputDigits = Left (InvalidDigit (head $ filter (\d -> d < 0 || d >= inputBase) inputDigits))
    | null inputDigits = Right []
    | otherwise = Right $ toDigits outputBase $ fromDigits inputBase inputDigits
  where
    -- Convert from digits in given base to decimal value
    fromDigits :: Integral a => a -> [a] -> a
    fromDigits base = foldl (\acc digit -> acc * base + digit) 0
    
    -- Convert from decimal value to digits in given base
    toDigits :: Integral a => a -> a -> [a]
    toDigits base n
        | n == 0 = [0]
        | otherwise = toDigits' base n
    
    toDigits' :: Integral a => a -> a -> [a]
    toDigits' base n
        | n == 0 = []
        | otherwise = toDigits' base quotient ++ [remainder]
        where (quotient, remainder) = n `divMod` base
