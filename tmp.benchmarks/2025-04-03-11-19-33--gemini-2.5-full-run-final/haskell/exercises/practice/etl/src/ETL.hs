module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toLower)

-- | Transforms the legacy Scrabble score format to the new format.
--
-- The legacy format maps point values to strings of uppercase letters.
-- The new format maps lowercase letters to their individual point values.
--
-- Example: transform (Map.fromList [(1, "AEIOU")]) == Map.fromList [('a', 1), ('e', 1), ('i', 1), ('o', 1), ('u', 1)]
transform :: Ord a         -- ^ Constraint: The point value type 'a' must be orderable for Map keys.
          => Map a String  -- ^ Input: Map from point value 'a' to a String of letters sharing that value.
          -> Map Char a    -- ^ Output: Map from lowercase character to its point value 'a'.
transform legacyData =
    Map.fromList [ (toLower letter, pointValue) -- Create a tuple (lowercase_letter, point_value)
                 | (pointValue, letters) <- Map.toList legacyData -- Iterate through (point_value, letters_string) pairs from the input map
                 , letter <- letters -- Iterate through each letter in the letters_string
                 ]
