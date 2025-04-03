module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = fromList $ concatMap transformEntry $ toList legacyData
  where
    transformEntry (score, letters) =
      [(toLower letter, score) | letter <- letters]
