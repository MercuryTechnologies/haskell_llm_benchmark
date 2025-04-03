module ETL (transform) where

import Data.Map (Map, fromList, toList)

transform :: Map Int String -> Map Char Int
transform legacyData = fromList $ concatMap toPairs (toList legacyData)
  where
    toPairs (score, letters) = [(toLower letter, score) | letter <- letters]
    toLower letter = if letter >= 'A' && letter <= 'Z' then toEnum (fromEnum letter + 32) else letter
