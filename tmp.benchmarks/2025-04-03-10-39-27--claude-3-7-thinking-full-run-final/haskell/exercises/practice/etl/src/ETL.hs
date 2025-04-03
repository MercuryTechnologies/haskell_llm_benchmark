module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toLower)

transform :: Map Int String -> Map Char Int
transform legacyData = 
    Map.foldlWithKey addEntries Map.empty legacyData
  where
    addEntries acc score letters = 
      foldr (\c m -> Map.insert (toLower c) score m) acc letters
