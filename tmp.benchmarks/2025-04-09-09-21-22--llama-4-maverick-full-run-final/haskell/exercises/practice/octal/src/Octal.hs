module Octal (readOct, showOct) where

readOct :: String -> Integer
readOct s = go s 0
  where
    go :: String -> Integer -> Integer
    go []     acc = acc
    go (c:cs) acc = acc `seq` go cs (acc * 8 + fromIntegral (digitToInt c))
    digitToInt :: Char -> Int
    digitToInt c
      | '0' <= c && c <= '7' = fromEnum c - fromEnum '0'
      | otherwise            = 0

showOct :: Integer -> String
showOct 0 = "0"
showOct n = go n []
  where
    go :: Integer -> String -> String
    go 0 s     = s
    go n s     = go (n `div` 8) (show (n `mod` 8) ++ s)
