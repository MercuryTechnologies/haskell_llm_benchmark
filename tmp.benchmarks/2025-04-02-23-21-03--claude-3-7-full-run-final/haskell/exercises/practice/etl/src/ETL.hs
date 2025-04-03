module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toLower)

transform :: Map Int String -> Map Char Int
transform legacyData = Map.fromList $ concatMap convertEntry $ Map.toList legacyData
  where
    convertEntry :: (Int, String) -> [(Char, Int)]
    convertEntry (score, letters) = [(toLower letter, score) | letter <- letters]
