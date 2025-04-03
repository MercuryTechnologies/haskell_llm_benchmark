module Roman (numerals) where

import Data.List (find)

-- Define the Roman numeral mappings, ordered from largest to smallest value.
-- Includes the subtractive forms (CM, CD, XC, XL, IX, IV).
romanMap :: [(Integer, String)]
romanMap =
  [ (1000, "M")
  , (900,  "CM")
  , (500,  "D")
  , (400,  "CD")
  , (100,  "C")
  , (90,   "XC")
  , (50,   "L")
  , (40,   "XL")
  , (10,   "X")
  , (9,    "IX")
  , (5,    "V")
  , (4,    "IV")
  , (1,    "I")
  ]

-- | Converts an Integer to its Roman numeral representation, if possible.
-- Returns Nothing for numbers outside the traditional range (1 to 3999).
numerals :: Integer -> Maybe String
numerals n
  -- Guard against invalid input: numbers less than 1 or greater than 3999.
  | n <= 0 || n >= 4000 = Nothing
  -- Otherwise, perform the conversion using the helper function.
  | otherwise             = Just (toRoman n)

-- | Helper function to convert a positive integer (within the valid range)
-- | to its Roman numeral string representation using recursion.
toRoman :: Integer -> String
-- Base case: If the number is 0, we're done.
toRoman 0 = ""
-- Recursive step:
toRoman n =
  -- Find the largest Roman numeral value less than or equal to the current number n.
  -- The 'find' function returns the first element satisfying the predicate.
  -- Since romanMap is sorted descendingly, this gives the largest value.
  case find (\(val, _) -> val <= n) romanMap of
    -- If a mapping is found (which it always should be for n > 0 with this map):
    Just (val, sym) ->
      -- Append the corresponding symbol and recurse with the remainder.
      sym ++ toRoman (n - val)
    -- This case should theoretically not be reached for n > 0 given the map includes 1.
    Nothing         -> error "Internal error: No matching Roman numeral value found."

