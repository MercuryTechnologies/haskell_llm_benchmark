module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = arePairedHelper xs []

arePairedHelper :: String -> [Char] -> Bool
arePairedHelper [] [] = True
arePairedHelper [] _ = False
arePairedHelper (x:xs) stack
  | x `elem` ['(', '[', '{'] = arePairedHelper xs (x:stack)
  | x == ')' = case stack of
                  ('(':rest) -> arePairedHelper xs rest
                  _ -> False
  | x == ']' = case stack of
                  ('[':rest) -> arePairedHelper xs rest
                  _ -> False
  | x == '}' = case stack of
                  ('{':rest) -> arePairedHelper xs rest
                  _ -> False
  | otherwise = arePairedHelper xs stack
