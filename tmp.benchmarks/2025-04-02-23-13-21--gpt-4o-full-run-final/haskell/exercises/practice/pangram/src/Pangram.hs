module Pangram (isPangram) where

import Data.Char (toLower)
import Data.List (nub)

isPangram :: String -> Bool
isPangram text = length (nub [toLower c | c <- text, c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z']) == 26
