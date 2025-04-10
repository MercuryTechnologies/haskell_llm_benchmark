module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)

transform :: Map Int String -> Map Char Int
transform legacyData = fromList [ (toLower c, score) | (score, letters) <- toList legacyData, c <- letters ]
