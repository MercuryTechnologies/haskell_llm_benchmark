module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = arePaired' []

arePaired' :: String -> String -> Bool
arePaired' [] [] = True
arePaired' _ [] = False
arePaired' stack (x:xs)
    | x `elem` "({[" = arePaired' (x:stack) xs
    | x `elem` ")}]" = not (null stack) && matches (head stack) x && arePaired' (tail stack) xs
    | otherwise = arePaired' stack xs

matches :: Char -> Char -> Bool
matches '(' ')' = True
matches '{' '}' = True
matches '[' ']' = True
matches _ _ = False
