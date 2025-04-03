module Octal (readOct, showOct) where

readOct :: String -> Integer
readOct s = go 0 s
  where
    go !acc [] = acc
    go !acc (x:xs)
      | x >= '0' && x <= '7' =
          let acc' = acc * 8 + toInteger (fromEnum x - fromEnum '0')
          in acc' `seq` go acc' xs
      | otherwise = 0

showOct :: Integer -> String
showOct n
  | n < 0     = error "Negative input not supported"
  | n == 0    = "0"
  | otherwise = go n []
  where
    go 0 acc = acc
    go !n acc =
      let (q, r) = n `quotRem` 8
          digit  = toEnum (fromEnum '0' + fromIntegral r) :: Char
      in q `seq` go q (digit : acc)
