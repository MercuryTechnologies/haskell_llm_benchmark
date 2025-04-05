module Isogram (isIsogram) where

import Data.Char (isLetter, toLower)
import qualified Data.Set as Set

isIsogram :: String -> Bool
isIsogram str = checkLetters (map toLower str) Set.empty
  where
    checkLetters [] _ = True
    checkLetters (c:cs) seen
      | not (isLetter c) = checkLetters cs seen
      | c `Set.member` seen = False
      | otherwise = checkLetters cs (Set.insert c seen)
