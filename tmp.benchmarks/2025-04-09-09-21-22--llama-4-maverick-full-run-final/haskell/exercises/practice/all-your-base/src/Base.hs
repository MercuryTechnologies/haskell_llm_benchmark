module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2 = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | otherwise = do
        decimal <- toDecimal inputBase inputDigits
        toBase outputBase decimal

toDecimal :: Integral a => a -> [a] -> Either (Error a) a
toDecimal base digits = go (reverse digits)
  where
    go [] = Right 0
    go (d:ds)
      | d < 0 || d >= base = Left (InvalidDigit d)
      | otherwise = do
          rest <- go ds
          return $ d + base * rest

toBase :: Integral a => a -> a -> Either (Error a) [a]
toBase _ 0 = Right []
toBase base n = Right (reverse (go n))
  where
    go 0 = []
    go x = (x `mod` base) : go (x `div` base)
