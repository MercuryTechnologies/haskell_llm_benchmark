module Base (Error(..), rebase) where

data Error a
  = InvalidInputBase
  | InvalidOutputBase
  | InvalidDigit a
  deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase < 2                 = Left InvalidInputBase
  | outputBase < 2                = Left InvalidOutputBase
  | any invalidDigit inputDigits  = Left (InvalidDigit (head (filter invalidDigit inputDigits)))
  | null inputDigits              = Right []
  | otherwise                     = Right (toBase (fromBase inputBase inputDigits) outputBase)
  where
    invalidDigit d = d < 0 || d >= inputBase

    fromBase b = foldl (\acc d -> acc * b + d) 0

    toBase 0 _ = [0]
    toBase n b = reverse (go n b)
      where
        go 0 _ = []
        go x base =
          let (q, r) = quotRem x base
          in r : go q base
