module Octal (readOct, showOct) where

-- Function to calculate the decimal equivalent of an octal number
readOct :: String -> Integer
readOct s = foldl (\acc d -> acc * 8 + fromIntegral (digitToInt d)) 0 s
  where
    digitToInt :: Char -> Int
    digitToInt c
      | '0' <= c && c <= '7' = fromEnum c - fromEnum '0'
      | otherwise            = 0

-- Function to convert a decimal number to its octal representation
showOct :: Integer -> String
showOct 0 = "0"
showOct n = showOct' n
  where
    showOct' 0 = ""
    showOct' x = showOct' (x `div` 8) ++ [toEnum (fromIntegral (x `mod` 8) + fromEnum '0')]
