module Binary (toDecimal) where

-- Function to check if a string represents a valid binary number
isValidBinary :: String -> Bool
isValidBinary [] = False -- An empty string is not considered a valid binary number here
isValidBinary xs = all (\c -> c == '0' || c == '1') xs

-- Converts a binary string representation to its decimal integer equivalent.
-- Returns 0 if the input string is invalid (empty or contains non-binary characters).
toDecimal :: String -> Int
toDecimal xs
  | not (isValidBinary xs) = 0
  | otherwise = foldl calculateValue 0 (zip (reverse xs) [0..])
  where
    calculateValue :: Int -> (Char, Int) -> Int
    calculateValue acc (digitChar, power) =
      acc + charToMultiplier digitChar * (2 ^ power)

    charToMultiplier :: Char -> Int
    charToMultiplier '1' = 1
    charToMultiplier '0' = 0
    -- This case should not be reached due to the isValidBinary check,
    -- but included for completeness.
    charToMultiplier _   = 0
