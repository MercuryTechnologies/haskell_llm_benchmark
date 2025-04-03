module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toLower)

-- | Transforms the legacy Scrabble score format to the new format.
--
-- The legacy format maps scores to lists of uppercase letters:
--   Map Int String
--   (e.g., Map.fromList [ (1, "AEIOU"), (2, "DG") ])
--
-- The new format maps lowercase letters to their scores:
--   Map Char Int
--   (e.g., Map.fromList [ ('a', 1), ('e', 1), ('i', 1), ('o', 1), ('u', 1), ('d', 2), ('g', 2) ])
--
-- The type parameter 'a' represents the score type, which must be orderable.
transform :: Ord a => Map a String -> Map Char a
transform legacyData = Map.fromList $ concatMap processEntry (Map.toList legacyData)
  where
    -- Helper function to process a single (score, letters) entry from the legacy map.
    -- It returns a list of (lowercase_letter, score) pairs.
    processEntry :: (a, String) -> [(Char, a)]
    processEntry (score, letters) = map (\letter -> (toLower letter, score)) letters
