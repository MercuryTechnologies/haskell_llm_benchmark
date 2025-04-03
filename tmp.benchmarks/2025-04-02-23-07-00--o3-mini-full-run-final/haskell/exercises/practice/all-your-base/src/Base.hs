module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase [] 
    | inputBase <= 1 = Left InvalidInputBase
    | outputBase <= 1 = Left InvalidOutputBase
    | otherwise = Right []
rebase inputBase outputBase inputDigits
    | inputBase <= 1 = Left InvalidInputBase
    | outputBase <= 1 = Left InvalidOutputBase
    | any isInvalid inputDigits = Left (InvalidDigit firstInvalid)
    | otherwise = Right (toDigits outputBase total)
  where
    isInvalid d = d < 0 || d >= inputBase
    firstInvalid = head [ d | d <- inputDigits, isInvalid d ]
    total = foldl (\acc d -> acc * inputBase + d) 0 inputDigits

    toDigits _ 0 = [0]
    toDigits base n = reverse (digits n)
      where
        digits 0 = []
        digits x = let (q, r) = quotRem x base in r : digits q
