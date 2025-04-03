module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite phrases =
  let pairs = zip phrases (tail phrases)
      lines1 = [ "For want of a " ++ w ++ " the " ++ n ++ " was lost." | (w, n) <- pairs ]
      finalLine = "And all for the want of a " ++ head phrases ++ "."
  in intercalate "\n" (lines1 ++ [finalLine])
