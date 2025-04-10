module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toLower)

-- | Transforms a legacy map of point values to uppercase letters
--   into a new map of lowercase letters to point values.
transform :: Map a String -> Map Char a
transform legacyData =
    Map.fromList
        [ (toLower c, score)
        | (score, letters) <- Map.toList legacyData
        , c <- letters
        ]
