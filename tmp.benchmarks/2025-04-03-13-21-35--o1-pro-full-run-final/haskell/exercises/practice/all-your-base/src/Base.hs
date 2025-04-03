module Base (Error(..), rebase) where

data Error a
    = InvalidInputBase
    | InvalidOutputBase
    | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2 = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | invalidDigitPresent =
        case filter (\d -> d < 0 || d >= inputBase) inputDigits of
          (x:_) -> Left (InvalidDigit x)
          []    -> error "Unexpected: invalidDigitPresent is True but no invalid digits found"
    | null inputDigits = Right []
    | otherwise =
        let number = foldl (\acc d -> acc * inputBase + d) 0 inputDigits
        in if number == 0
           then Right [0]
           else Right (toOutputBase number outputBase)
  where
    invalidDigitPresent = any (\d -> d < 0 || d >= inputBase) inputDigits

    toOutputBase :: Integral a => a -> a -> [a]
    toOutputBase 0 _ = []
    toOutputBase n b = toOutputBase (n `div` b) b ++ [n `mod` b]
