module Hexadecimal (hexToInt) where

import Data.Char (ord, toLower)

-- | Convert a single hexadecimal character to its integer value.
-- Returns Nothing if the character is not a valid hex digit.
hexCharToInt :: Char -> Maybe Int
hexCharToInt c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | 'a' <= lc && lc <= 'f' = Just (10 + ord lc - ord 'a')
  | otherwise = Nothing
  where lc = toLower c

-- | Convert a hexadecimal string to its decimal integer value.
-- Returns 0 if the string contains any invalid characters.
hexToInt :: String -> Int
hexToInt = go 0
  where
    go acc [] = acc
    go acc (x:xs) =
      case hexCharToInt x of
        Just n  -> go (acc * 16 + n) xs
        Nothing -> 0
