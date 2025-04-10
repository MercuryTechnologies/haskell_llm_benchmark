module Binary (toDecimal) where

import Data.Char (digitToInt)

-- | Converts a binary string to its decimal equivalent.
--   Returns Nothing for invalid input (non-binary characters).
toDecimal :: String -> Int
toDecimal xs
  | all (`elem` "01") xs = foldl (\acc x -> acc * 2 + digitToInt x) 0 xs
  | otherwise            = 0
