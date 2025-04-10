module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

-- | Convert a list of digits in inputBase to a list of digits in outputBase.
-- Returns an error if the input or output base is invalid, or if any digit is invalid.
rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2 = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | otherwise =
        case checkDigits inputBase inputDigits of
            Just bad -> Left (InvalidDigit bad)
            Nothing  -> Right (convert inputDigits)
  where
    -- Check for invalid digits; return the first invalid digit if any.
    checkDigits b = go
      where
        go [] = Nothing
        go (d:ds)
            | d < 0 || d >= b = Just d
            | otherwise = go ds

    -- Convert a list of digits in base b to an Integer.
    fromBase b = foldl (\acc d -> acc * b + d) 0

    -- Convert an Integer to a list of digits in base b.
    toBase b 0 = [0]
    toBase b n = reverse (toBase' n)
      where
        toBase' 0 = []
        toBase' x = let (q, r) = quotRem x b in fromIntegral r : toBase' q

    -- The main conversion logic, handling the empty list case.
    convert [] = []
    convert xs =
        let n = fromBase inputBase xs
        in if n == 0 then [] else toBase outputBase n
