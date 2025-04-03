module Isogram (isIsogram) where

import Data.Char (toLower)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram xs =
  let filtered = [toLower ch | ch <- xs, ch /= ' ' && ch /= '-']
  in length filtered == length (nub filtered)
