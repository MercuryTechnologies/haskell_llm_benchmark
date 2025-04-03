module Octal (readOct, showOct) where

readOct :: String -> Integer
readOct s = foldl (\acc c -> let digit = charToInt c in
                                 if digit >= 0 && digit <= 7
                                 then acc * 8 + digit
                                 else 0) 0 s
  where
    charToInt c = fromEnum c - fromEnum '0'

showOct :: Integer -> String
showOct 0 = "0"
showOct n = reverse $ toOct n
  where
    toOct 0 = []
    toOct x = let (q, r) = x `quotRem` 8 in
                (intToChar r) : toOct q
    intToChar d = toEnum (fromEnum '0' + fromInteger d)

