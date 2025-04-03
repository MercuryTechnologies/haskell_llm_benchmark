module Pangram (isPangram) where

import Data.Char (toLower)
import Data.List (nub)

isPangram :: String -> Bool
isPangram text = all (`elem` lowerText) ['a'..'z']
  where lowerText = map toLower text
