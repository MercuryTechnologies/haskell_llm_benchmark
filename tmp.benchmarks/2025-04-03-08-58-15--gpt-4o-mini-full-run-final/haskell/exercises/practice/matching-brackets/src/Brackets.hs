module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = arePaired' xs [] where
    arePaired' [] [] = True
    arePaired' [] _ = False
    arePaired' (x:xs) stack
        | x `elem` "([{{" = arePaired' xs (x:stack)
        | x `elem` ")]}]" = case stack of
            (s:rest) | matches s x -> arePaired' xs rest
            _ -> False
        | otherwise = arePaired' xs stack

    matches '(' ')' = True
    matches '[' ']' = True
    matches '{' '}' = True
    matches _ _ = False
