module Proverb(recite) where

recite :: [String] -> String
recite xs
  | null xs = "No pieces to recite.\n"
  | otherwise = concatMap createLine (zip xs (tail xs)) ++ lastLine
  where
    createLine (a, b) = "For want of a " ++ a ++ " the " ++ b ++ " was lost.\n"
    lastLine = "And all for the want of a " ++ head xs ++ ".\n"
