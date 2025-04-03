module Octal (readOct, showOct) where

import Prelude

-- Helper to check if a character is a valid octal digit ('0'..'7')
isValidOctalChar :: Char -> Bool
isValidOctalChar c = c >= '0' && c <= '7'

-- Helper to convert a valid octal character to its integer value.
-- Assumes the character has already been validated.
octalCharToInt :: Char -> Int
octalCharToInt c = fromEnum c - fromEnum '0' -- fromEnum is in Prelude

-- Manual strict left fold using seq for efficiency, as hinted.
foldlStrict :: (b -> a -> b) -> b -> [a] -> b
foldlStrict _ z []     = z -- Fixed unused variable warning: f -> _
foldlStrict f z (x:xs) = let z' = f z x in z' `seq` foldlStrict f z' xs

-- | Converts an octal string representation to its decimal integer equivalent.
-- Treats invalid input (non-octal digits or empty string) as octal 0.
readOct :: String -> Integer -- Changed Int to Integer
readOct s
  -- Use `all` to check if every character in the string is a valid octal digit.
  | all isValidOctalChar s = foldlStrict step (0 :: Integer) s -- Use Integer 0
  -- If any character is invalid, return 0 as per requirements.
  | otherwise              = 0
  where
    -- The accumulator function for the fold.
    -- Multiplies the current accumulated value by 8 (shifting digits left)
    -- and adds the value of the new digit.
    step :: Integer -> Char -> Integer -- Changed Int to Integer
    step acc c = acc * 8 + toInteger (octalCharToInt c) -- Convert digit value to Integer

-- Helper to convert an Integer (0-7) to its octal Char ('0'-'7')
-- Uses functions from Prelude.
intToOctalChar :: Integer -> Char
intToOctalChar i
  | i >= 0 && i <= 7 = toEnum (fromEnum '0' + fromIntegral i) -- fromIntegral converts Integer to Int for '+'
  | otherwise        = error "intToOctalChar: digit out of range 0-7"

-- | Converts a non-negative decimal integer to its octal string representation.
showOct :: Integer -> String -- Changed Int to Integer
showOct 0 = "0" -- Base case for 0
showOct n
  | n < 0     = error "showOct: Input must be non-negative" -- Test suite uses Positive, so this shouldn't be hit by tests
  | otherwise = reverse (go n) -- Build digits in reverse order, then reverse the list
  where
    -- Recursively generates octal digits (in reverse order)
    go :: Integer -> String
    go 0 = "" -- Stop when the number becomes 0
    go x = intToOctalChar (x `mod` 8) : go (x `div` 8) -- Prepend the char for the last octal digit and recurse
