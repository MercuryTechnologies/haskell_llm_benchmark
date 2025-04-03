module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2 || outputBase < 2 = Left InvalidInputBase
    | any (\d -> d < 0 || d >= inputBase) inputDigits = Left (InvalidDigit (head (filter (\d -> d < 0 || d >= inputBase) inputDigits)))
    | null inputDigits = Right []  -- Handle empty input list case
    | otherwise = Right (convertToOutputBase outputBase (toDecimal inputBase inputDigits))

toDecimal :: Integral a => a -> [a] -> a
toDecimal base digits = sum $ zipWith (\d p -> d * (base ^ p)) (reverse digits) [0..]

convertToOutputBase :: Integral a => a -> a -> [a]
convertToOutputBase base 0 = [0]
convertToOutputBase base number = reverse (convert number)
  where
    convert 0 = []
    convert n = let (q, r) = n `divMod` base in r : convert q
