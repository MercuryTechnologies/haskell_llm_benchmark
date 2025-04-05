module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase <= 1 = Left InvalidInputBase
    | outputBase <= 1 = Left InvalidOutputBase
    | any (< 0) inputDigits || any (>= inputBase) inputDigits =
        case filter (\d -> d < 0 || d >= inputBase) inputDigits of
            (d:_) -> Left (InvalidDigit d)
            [] -> Left InvalidInputBase  -- Should not happen
    | null inputDigits = Right []
    | all (== 0) inputDigits = Right []
    | otherwise = Right (toBase outputBase (fromBase inputBase inputDigits))
  where
    fromBase :: Integral a => a -> [a] -> a
    fromBase base = foldl (\acc d -> acc * base + d) 0

    toBase :: Integral a => a -> a -> [a]
    toBase base 0 = []
    toBase base n = reverse (toBase' n)
      where
        toBase' 0 = []
        toBase' x = let (q, r) = quotRem x base in r : toBase' q
