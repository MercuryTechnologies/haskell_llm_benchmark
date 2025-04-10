module Isogram (isIsogram) where

import Data.Char (isAlpha, toLower)
import qualified Data.Set as Set

isIsogram :: String -> Bool
isIsogram str = isIsogram' (map toLower $ filter isAlpha str) Set.empty
  where
    isIsogram' [] _ = True
    isIsogram' (x:xs) seen
      | x `Set.member` seen = False
      | otherwise           = isIsogram' xs (Set.insert x seen)
