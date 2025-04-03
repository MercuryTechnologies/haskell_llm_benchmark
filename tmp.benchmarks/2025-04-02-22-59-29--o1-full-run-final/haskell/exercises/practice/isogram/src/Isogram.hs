module Isogram (isIsogram) where

import Data.Char (toLower)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram str =
    let normalized = [toLower c | c <- str, c /= ' ' && c /= '-']
    in length (nub normalized) == length normalized
