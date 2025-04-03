module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = checkBrackets xs []

-- Helper function that uses a stack to track opening brackets
checkBrackets :: String -> [Char] -> Bool
checkBrackets [] [] = True           -- Empty string and empty stack means all brackets are matched
checkBrackets [] (_:_) = False       -- Empty string but non-empty stack means unmatched opening brackets
checkBrackets (x:xs) stack
  | x `elem` "([{" = checkBrackets xs (x:stack)  -- Push opening brackets onto stack
  | x `elem` ")]}" = case stack of
      [] -> False                    -- Closing bracket with empty stack means unmatched closing bracket
      (top:rest) -> isMatchingPair top x && checkBrackets xs rest  -- Check if matching pair and continue
  | otherwise = checkBrackets xs stack  -- Ignore non-bracket characters

-- Check if two brackets form a matching pair
isMatchingPair :: Char -> Char -> Bool
isMatchingPair '(' ')' = True
isMatchingPair '[' ']' = True
isMatchingPair '{' '}' = True
isMatchingPair _ _ = False
