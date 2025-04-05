module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = check xs []
  where
    check [] [] = True
    check [] _  = False
    check (c:cs) stack
      | c `elem` "([{"
          = check cs (c:stack)
      | c `elem` ")]}"
          = case stack of
              (s:ss) | matches s c -> check cs ss
              _ -> False
      | otherwise = check cs stack

    matches '(' ')' = True
    matches '[' ']' = True
    matches '{' '}' = True
    matches _ _ = False
