module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toLower)

transform :: (Ord a, Num a) => Map a String -> Map Char a
transform legacyData =
  Map.fromList [ (toLower c, score) | (score, letters) <- Map.toList legacyData, c <- letters ]
