module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = 
    fromList [(toLower c, score) | (score, letters) <- toList legacyData, c <- letters]
