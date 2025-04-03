module Hexadecimal (hexToInt) where

import Data.Char (ord, toLower)

-- Helper function to convert a single hex character to its integer value.
-- Returns Nothing if the character is not a valid hex digit (case-insensitive).
hexCharToInt :: Char -> Maybe Int
hexCharToInt c
    | lc >= '0' && lc <= '9' = Just (ord lc - ord '0')
    | lc >= 'a' && lc <= 'f' = Just (ord lc - ord 'a' + 10)
    | otherwise              = Nothing
  where
    lc = toLower c

-- Converts a hexadecimal string to its decimal integer representation.
-- Returns 0 if the string contains invalid characters or is empty.
hexToInt :: String -> Int
hexToInt hexString =
    case mapM hexCharToInt hexString of
        Nothing    -> 0 -- Invalid character found, return 0
        Just []    -> 0 -- Empty string is valid, value is 0
        Just digits -> foldl calculateValue 0 digits
  where
    -- Accumulates the decimal value by processing digits from left to right.
    calculateValue :: Int -> Int -> Int
    calculateValue accumulator digit = accumulator * 16 + digit
