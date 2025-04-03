module Hexadecimal (hexToInt) where

import Data.Char (toLower)
import Data.List (foldl')
import Data.Maybe (fromMaybe)

-- | Converts a single hexadecimal character to its integer value (0-15).
-- Returns Nothing if the character is not a valid hexadecimal digit.
-- Case-insensitive.
hexCharToInt :: Char -> Maybe Int
hexCharToInt c = case toLower c of
  '0' -> Just 0
  '1' -> Just 1
  '2' -> Just 2
  '3' -> Just 3
  '4' -> Just 4
  '5' -> Just 5
  '6' -> Just 6
  '7' -> Just 7
  '8' -> Just 8
  '9' -> Just 9
  'a' -> Just 10
  'b' -> Just 11
  'c' -> Just 12
  'd' -> Just 13
  'e' -> Just 14
  'f' -> Just 15
  _   -> Nothing

-- | Converts a hexadecimal string representation to its decimal integer value.
-- Returns 0 if the input string is invalid.
-- Implemented using first principles.
hexToInt :: String -> Int
hexToInt hexString = fromMaybe 0 (go hexString)
  where
    -- Helper function using Maybe monad to handle potential failures
    go :: String -> Maybe Int
    go s = do
      -- Attempt to convert every character to its integer value.
      -- If any character is invalid, mapM returns Nothing.
      digits <- mapM hexCharToInt s
      -- If all characters were valid, calculate the decimal value using foldl'.
      -- foldl' (\acc digit -> acc * 16 + digit) 0 [d1, d2, d3] calculates
      -- ((0 * 16 + d1) * 16 + d2) * 16 + d3 = d1*16^2 + d2*16^1 + d3*16^0
      return $ foldl' (\acc digit -> acc * 16 + digit) 0 digits
