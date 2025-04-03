module Roman (numerals) where

-- Defines the mapping from Arabic values to Roman numeral symbols.
-- The list is ordered by value in descending order to facilitate a greedy algorithm.
romanMap :: [(Integer, String)]
romanMap =
  [ (1000, "M")
  , (900, "CM")
  , (500, "D")
  , (400, "CD")
  , (100, "C")
  , (90, "XC")
  , (50, "L")
  , (40, "XL")
  , (10, "X")
  , (9, "IX")
  , (5, "V")
  , (4, "IV")
  , (1, "I")
  ]

-- | Converts an Integer to its Roman numeral representation, if possible.
-- Returns Nothing for numbers outside the traditional range (1 to 3999).
numerals :: Integer -> Maybe String
numerals n
  -- Guard against invalid inputs (non-positive or too large for traditional Roman numerals).
  | n <= 0 || n >= 4000 = Nothing
  -- If the input is valid, proceed with the conversion.
  | otherwise           = Just (toRoman n romanMap)
  where
    -- Helper function to recursively build the Roman numeral string.
    toRoman :: Integer -> [(Integer, String)] -> String
    -- Base case: If the number is 0, the conversion is complete.
    toRoman 0 _      = ""
    -- Recursive step: Process the current number with the map.
    toRoman num remainingMap@((val, sym):rest)
      -- If the current number is greater than or equal to the map value,
      -- append the symbol and recurse with the reduced number and the *same* map.
      -- This allows for repeated symbols (e.g., III).
      | num >= val = sym ++ toRoman (num - val) remainingMap
      -- If the current number is smaller than the map value,
      -- move to the next (smaller) value in the map.
      | otherwise  = toRoman num rest
    -- Base case for the map (should not be reached with valid positive input and the current map).
    toRoman _ [] = ""
