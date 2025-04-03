module Octal (readOct, showOct) where

-- | Convert an octal string to a decimal integer
-- Invalid input is treated as octal 0
readOct :: String -> Int
readOct [] = 0
readOct str = if all isOctalDigit str
              then foldl accumulate 0 str
              else 0
  where
    isOctalDigit c = c >= '0' && c <= '7'
    accumulate acc digit = acc * 8 + charToInt digit
    charToInt c = fromEnum c - fromEnum '0'

-- | Convert a decimal integer to an octal string
showOct :: Int -> String
showOct 0 = "0"
showOct n
  | n < 0     = '-' : showOct (negate n)
  | otherwise = go n ""
  where
    go 0 acc = acc
    go m acc = go (m `div` 8) (intToChar (m `mod` 8) : acc)
    intToChar i = toEnum (fromEnum '0' + i)
