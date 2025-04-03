module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = go []
  where
    go stack [] = null stack
    go stack (x:xs)
      | x == '(' || x == '[' || x == '{' = go (x:stack) xs
      | x == ')' = case stack of
          ('(':rest) -> go rest xs
          _          -> False
      | x == ']' = case stack of
          ('[':rest) -> go rest xs
          _          -> False
      | x == '}' = case stack of
          ('{':rest) -> go rest xs
          _          -> False
      | otherwise = go stack xs
