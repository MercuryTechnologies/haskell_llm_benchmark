module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = checkBrackets []

checkBrackets :: [Char] -> String -> Bool
checkBrackets [] [] = True
checkBrackets _ [] = False
checkBrackets stack (x:xs)
  | isOpen x = checkBrackets (x:stack) xs
  | isClose x = case stack of
                 (s:ss) -> matching s x && checkBrackets ss xs
                 [] -> False
  | otherwise = checkBrackets stack xs

isOpen :: Char -> Bool
isOpen c = c `elem` "([{"

isClose :: Char -> Bool
isClose c = c `elem` ")]}"

matching :: Char -> Char -> Bool
matching '(' ')' = True
matching '[' ']' = True
matching '{' '}' = True
matching _ _ = False
