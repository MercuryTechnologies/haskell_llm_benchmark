module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase <= 1 = Left InvalidInputBase
  | outputBase <= 1 = Left InvalidOutputBase
  | any (< 0) inputDigits = Left $ InvalidDigit $ head $ filter (< 0) inputDigits
  | any (>= inputBase) inputDigits = Left $ InvalidDigit $ head $ filter (>= inputBase) inputDigits
  | otherwise = Right $ convert inputBase outputBase inputDigits

convert :: Integral a => a -> a -> [a] -> [a]
convert inputBase outputBase inputDigits =
  let
    toDecimal :: Integral a => a -> [a] -> a
    toDecimal base digits = foldl (\acc digit -> acc * base + digit) 0 digits

    fromDecimal :: Integral a => a -> a -> a -> [a]
    fromDecimal base num
      | num == 0  = [0]
      | otherwise = reverse (go base num [])
      where
        go b n acc =
          if n == 0
            then acc
            else let (q, r) = quotRem n b in go b q (r : acc)

    decimalValue = toDecimal inputBase inputDigits
    result = fromDecimal outputBase decimalValue
  in
    if all (==0) inputDigits && not (null inputDigits) then [0] else if decimalValue == 0 then [0] else result
