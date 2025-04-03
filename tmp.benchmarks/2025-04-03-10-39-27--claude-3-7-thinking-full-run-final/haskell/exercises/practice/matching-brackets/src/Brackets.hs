module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = go []
  where
    go [] [] = True
    go (_:_) [] = False  -- Unmatched opening brackets
    go [] (x:xs)
      | x `elem` "([{" = go [x] xs
      | x `elem` ")]}" = False  -- Closing bracket with no matching opening bracket
      | otherwise = go [] xs  -- Ignore non-bracket
    go (s:ss) (x:xs)
      | x `elem` "([{" = go (x:s:ss) xs
      | x `elem` ")]}" && matchingPair s x = go ss xs
      | x `elem` ")]}" = False  -- Mismatched closing bracket
      | otherwise = go (s:ss) xs  -- Ignore non-bracket
    
    matchingPair '(' ')' = True
    matchingPair '[' ']' = True
    matchingPair '{' '}' = True
    matchingPair _ _ = False
