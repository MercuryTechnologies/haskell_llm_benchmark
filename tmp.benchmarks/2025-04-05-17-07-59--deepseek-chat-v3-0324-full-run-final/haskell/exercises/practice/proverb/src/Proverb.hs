module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite [x] = finalLine x
recite items@(x:_) = unlines (verseLines items ++ [finalLine x])
  where
    verseLines [] = []
    verseLines [_] = []
    verseLines (x1:x2:xs) = ("For want of a " ++ x1 ++ " the " ++ x2 ++ " was lost.") : verseLines (x2:xs)
    finalLine item = "And all for the want of a " ++ item ++ "."
