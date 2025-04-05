module ETL (transform) where

import Data.Char (toLower)
import qualified Data.Map as M
import Data.Map (Map)

transform :: Map Int [Char] -> Map Char Int
transform legacyData = M.fromList
    [ (toLower c, score)
    | (score, letters) <- M.toList legacyData
    , c <- letters
    ]
