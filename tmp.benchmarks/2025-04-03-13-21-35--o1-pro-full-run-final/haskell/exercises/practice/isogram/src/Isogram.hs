module Isogram (isIsogram) where

import Data.Char (toLower, isAlpha)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram xs =
    let filtered = [ toLower c | c <- xs, isAlpha c ]
    in length (nub filtered) == length filtered
