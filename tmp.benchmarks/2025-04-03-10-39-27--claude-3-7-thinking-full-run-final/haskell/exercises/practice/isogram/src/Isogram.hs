module Isogram (isIsogram) where

import Data.Char (toLower, isAlpha)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram str = 
  let lowercased = map toLower $ filter isAlpha str
  in length lowercased == length (nub lowercased)
