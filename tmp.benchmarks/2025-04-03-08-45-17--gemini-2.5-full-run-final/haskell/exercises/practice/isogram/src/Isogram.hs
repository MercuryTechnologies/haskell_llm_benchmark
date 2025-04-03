module Isogram (isIsogram) where

import Data.Char (toLower, isLetter)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram phrase =
  let cleaned = map toLower $ filter isLetter phrase
  in length cleaned == length (nub cleaned)
