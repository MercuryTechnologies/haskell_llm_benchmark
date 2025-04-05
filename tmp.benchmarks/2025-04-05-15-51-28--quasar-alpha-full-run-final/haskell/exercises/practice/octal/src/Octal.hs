module Octal (readOct, showOct) where

-- Convert an octal string to decimal Integer
readOct :: String -> Integer
readOct s = go s 0
  where
    go [] acc = acc
    go (c:cs) acc
      | c >= '0' && c <= '7' =
          let digit = toInteger (fromEnum c - fromEnum '0')
              acc' = acc * 8 + digit
          in acc' `seq` go cs acc'
      | otherwise = 0  -- invalid input, treat as 0

-- Convert a decimal Integer to an octal string
showOct :: Integer -> String
showOct n
  | n < 0     = error "Negative numbers not supported"
  | n == 0    = "0"
  | otherwise = reverse (toOct n)
  where
    toOct 0 = ""
    toOct x =
      let (q, r) = x `quotRem` 8
      in toEnum (fromInteger r + fromEnum '0') : toOct q
