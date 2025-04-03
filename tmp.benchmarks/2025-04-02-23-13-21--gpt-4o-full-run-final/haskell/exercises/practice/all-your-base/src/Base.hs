module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase <= 1 = Left InvalidInputBase
    | outputBase <= 1 = Left InvalidOutputBase
    | any (< 0) inputDigits || any (>= inputBase) inputDigits = Left (InvalidDigit (head (filter (\d -> d < 0 || d >= inputBase) inputDigits)))
    | otherwise = Right (toBase outputBase (fromBase inputBase inputDigits))

fromBase :: Integral a => a -> [a] -> a
fromBase base = foldl (\acc x -> acc * base + x) 0

toBase :: Integral a => a -> a -> [a]
toBase base 0 = []
toBase base n = toBase base (n `div` base) ++ [n `mod` base]
