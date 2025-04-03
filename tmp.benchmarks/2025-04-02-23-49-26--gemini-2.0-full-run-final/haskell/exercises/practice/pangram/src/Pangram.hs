module Pangram (isPangram) where

import Data.Char (toLower, isLetter)
import Data.Set (fromList, size)

isPangram :: String -> Bool
isPangram text =
  let
    letters = fromList $ map toLower $ filter isLetter text
  in
    size letters == 26
