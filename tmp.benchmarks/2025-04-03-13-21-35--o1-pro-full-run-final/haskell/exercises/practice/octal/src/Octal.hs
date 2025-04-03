{-# LANGUAGE BangPatterns #-}
module Octal (readOct, showOct) where

readOct :: String -> Integer
readOct str
  | any invalid str = 0
  | otherwise       = go 0 str
  where
    invalid c = c < '0' || c > '7'
    go !acc []     = acc
    go !acc (x:xs) = go (acc * 8 + digitValue x) xs
    digitValue c   = toInteger (fromEnum c - fromEnum '0')

showOct :: Integer -> String
showOct n
  | n == 0    = "0"
  | n < 0     = '-' : reverse (toOct (abs n) "")
  | otherwise = reverse (toOct n "")
  where
    toOct 0 res = res
    toOct x res =
      let (q, r) = x `divMod` 8
          c      = toEnum (fromEnum '0' + fromIntegral r)
      in toOct q (c:res)
