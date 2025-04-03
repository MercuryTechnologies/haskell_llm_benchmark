module Pangram (isPangram) where

import Data.Char (toLower, isAlpha)
import Data.Set (fromList, size)

isPangram :: String -> Bool
isPangram text = size (fromList [toLower c | c <- text, isAlpha c]) == 26
