module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Text (Text)

transform :: Map a String -> Map Char a
transform legacyData = fromList [(toLower letter, score) | (score, letters) <- toList legacyData, letter <- letters]
