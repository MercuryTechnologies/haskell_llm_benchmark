module Isogram (isIsogram) where

import Data.Char (toLower, isLetter)
import Data.List (nub)

-- | Determine if a word or phrase is an isogram.
-- An isogram is a word or phrase without a repeating letter,
-- however spaces and hyphens are allowed to appear multiple times.
-- Case is ignored.
isIsogram :: String -> Bool
isIsogram s =
  let relevantChars = map toLower $ filter isLetter s
  in length relevantChars == length (nub relevantChars)
