module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = check []
  where
    check :: String -> String -> Bool
    check stack [] = null stack
    check stack (x:xs)
      | x `elem` "([{"
        = check (x:stack) xs
      | x `elem` ")]}"
        = case stack of
            []      -> False
            (s:rest) -> if match s x then check rest xs else False
      | otherwise
        = check stack xs

    match :: Char -> Char -> Bool
    match '(' ')' = True
    match '[' ']' = True
    match '{' '}' = True
    match _   _   = False
