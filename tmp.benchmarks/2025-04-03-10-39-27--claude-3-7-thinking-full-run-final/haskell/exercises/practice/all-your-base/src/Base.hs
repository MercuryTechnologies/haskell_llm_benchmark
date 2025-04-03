module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase <= 1 = Left InvalidInputBase
    | outputBase <= 1 = Left InvalidOutputBase
    | any (< 0) inputDigits || any (>= inputBase) inputDigits = 
        Left $ InvalidDigit (head $ filter (\d -> d < 0 || d >= inputBase) inputDigits)
    | null inputDigits = Right []  -- Special case for empty list
    | otherwise = Right $ toBase outputBase $ fromBase inputBase inputDigits

-- Convert from any base to decimal (base 10)
fromBase :: Integral a => a -> [a] -> a
fromBase base digits = foldl (\acc digit -> acc * base + digit) 0 digits

-- Convert from decimal (base 10) to any base
toBase :: Integral a => a -> a -> [a]
toBase _ 0 = [0]
toBase base n = reverse $ toBase' n
  where
    toBase' 0 = []
    toBase' m = (m `mod` base) : toBase' (m `div` base)  -- Changed 'n' to 'm' to avoid shadowing
