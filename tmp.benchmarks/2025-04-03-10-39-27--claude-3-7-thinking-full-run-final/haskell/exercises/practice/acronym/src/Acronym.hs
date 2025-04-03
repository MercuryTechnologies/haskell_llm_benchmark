module Acronym (abbreviate) where

import Data.Char (toUpper, isPunctuation, isUpper)

abbreviate :: String -> String
abbreviate xs = map toUpper $ concatMap (take 1) $ words $ addSpacesBeforeUppercase $ processString xs
  where
    processString :: String -> String
    processString [] = []
    processString (c:cs)
      | c == '-' = ' ' : processString cs
      | isPunctuation c = processString cs
      | otherwise = c : processString cs
    
    addSpacesBeforeUppercase :: String -> String
    addSpacesBeforeUppercase [] = []
    addSpacesBeforeUppercase [c] = [c]
    addSpacesBeforeUppercase (c1:c2:cs)
      | not (isUpper c1) && isUpper c2 = c1 : ' ' : c2 : addSpacesBeforeUppercase cs
      | otherwise = c1 : addSpacesBeforeUppercase (c2:cs)
