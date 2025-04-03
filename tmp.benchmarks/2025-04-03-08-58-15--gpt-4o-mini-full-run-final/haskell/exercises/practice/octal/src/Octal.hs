module Octal (readOct, showOct) where

readOct :: String -> Integer
readOct str = go str 0 0
  where
    go [] _ acc = acc
    go (x:xs) pos acc
      | digit < 0 || digit > 7 = 0
      | otherwise = go xs (pos + 1) (acc + digit * (8 ^ pos))
      where digit = fromEnum x - fromEnum '0'

showOct :: Integer -> String
showOct = error "You need to implement this function."
