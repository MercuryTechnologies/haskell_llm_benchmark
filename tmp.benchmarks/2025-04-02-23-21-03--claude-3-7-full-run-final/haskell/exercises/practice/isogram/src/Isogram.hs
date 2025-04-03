module Isogram (isIsogram) where

import Data.Char (toLower)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram str = length relevantChars == length (nub relevantChars)
  where
    relevantChars = [toLower c | c <- str, c `notElem` [' ', '-']]
