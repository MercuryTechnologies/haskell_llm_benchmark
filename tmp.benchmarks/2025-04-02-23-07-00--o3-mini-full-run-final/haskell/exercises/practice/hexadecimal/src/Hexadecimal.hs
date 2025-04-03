module Hexadecimal (hexToInt) where

import Data.Char (ord, toLower)

-- | Convert a hexadecimal string to its decimal equivalent.
-- Throws an error if the string is empty or contains invalid characters.
hexToInt :: String -> Integer
hexToInt "" = error "Empty string"
hexToInt xs = foldl step 0 xs
  where
    step :: Integer -> Char -> Integer
    step n c = n * 16 + digit
      where
        digit = case charToDigit c of
                  Just d  -> d
                  Nothing -> error $ "Invalid hexadecimal digit: " ++ [c]

    charToDigit :: Char -> Maybe Integer
    charToDigit ch
      | '0' <= ch && ch <= '9' = Just (toInteger (ord ch - ord '0'))
      | 'a' <= ch' && ch' <= 'f' = Just (toInteger (ord ch' - ord 'a' + 10))
      | otherwise = Nothing
      where
        ch' = toLower ch
