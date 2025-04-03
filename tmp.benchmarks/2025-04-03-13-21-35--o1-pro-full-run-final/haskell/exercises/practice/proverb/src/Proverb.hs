module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite xs =
  intercalate "\n" (map makeLine (zip xs (tail xs)) ++ [finalLine])
  where
    makeLine (a, b) = "For want of a " ++ a ++ " the " ++ b ++ " was lost."
    finalLine       = "And all for the want of a " ++ head xs ++ "."
