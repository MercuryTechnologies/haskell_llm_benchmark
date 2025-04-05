module Isogram (isIsogram) where

import Data.Char (toLower, isLetter)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram str = letters == nub letters
  where
    letters = map toLower $ filter isRelevant str
    isRelevant c = isLetter c || c == '-' || c == ' '
