module Hexadecimal (hexToInt) where

-- | Convert a hexadecimal string to an integer
-- Returns 0 for invalid input
hexToInt :: String -> Int
hexToInt [] = 0
hexToInt str = case traverse hexCharToInt str of
  Nothing -> 0  -- Invalid input
  Just digits -> foldl (\acc d -> acc * 16 + d) 0 digits

-- | Convert a single hexadecimal character to its decimal value
hexCharToInt :: Char -> Maybe Int
hexCharToInt c
  | c >= '0' && c <= '9' = Just (fromEnum c - fromEnum '0')
  | c >= 'a' && c <= 'f' = Just (10 + fromEnum c - fromEnum 'a')
  | c >= 'A' && c <= 'F' = Just (10 + fromEnum c - fromEnum 'A')
  | otherwise            = Nothing
