module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite items@(first:_) = case items of
  [_] -> finalLine first
  _   -> unlines (generateLines items) ++ finalLine first
  where
    generateLines :: [String] -> [String]
    generateLines [] = []
    generateLines [_] = []
    generateLines (x:y:rest) = 
      ("For want of a " ++ x ++ " the " ++ y ++ " was lost.") : generateLines (y:rest)
    
    finalLine :: String -> String
    finalLine word = "And all for the want of a " ++ word ++ "."
