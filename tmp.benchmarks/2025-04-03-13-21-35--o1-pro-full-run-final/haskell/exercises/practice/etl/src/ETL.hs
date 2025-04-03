module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Char (toLower)

transform :: Ord a => Map a String -> Map Char a
transform legacyData =
    M.fromList
        [ (toLower letter, score)
        | (score, letters) <- M.toList legacyData
        , letter <- letters
        ]
