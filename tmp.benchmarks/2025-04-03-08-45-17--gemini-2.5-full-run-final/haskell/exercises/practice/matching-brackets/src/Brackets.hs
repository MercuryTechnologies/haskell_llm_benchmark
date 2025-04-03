module Brackets (arePaired) where

import Data.Maybe (isNothing, fromJust)

-- | Determines if a character is an opening bracket.
isOpenBracket :: Char -> Bool
isOpenBracket c = c `elem` "([{"

-- | Determines if a character is a closing bracket.
isCloseBracket :: Char -> Bool
isCloseBracket c = c `elem` ")]}"

-- | Returns the matching opening bracket for a given closing bracket.
matchingBracket :: Char -> Maybe Char
matchingBracket ')' = Just '('
matchingBracket ']' = Just '['
matchingBracket '}' = Just '{'
matchingBracket _   = Nothing

-- | Processes the input string using a stack to check for balanced brackets.
-- The stack holds the expected closing brackets.
arePaired :: String -> Bool
arePaired = go []
  where
    -- | Helper function that processes the string recursively.
    -- Takes the current stack and the remaining string as input.
    go :: String -> String -> Bool
    go stack [] = null stack -- If the string is empty, check if the stack is also empty.
    go stack (x:xs)
      | isOpenBracket x = go (openToClose x : stack) xs -- Push expected closing bracket onto stack.
      | isCloseBracket x =
          case stack of
            [] -> False -- Closing bracket with no opening bracket on stack.
            (s:ss) ->
              if Just x == Just s -- Check if closing bracket matches the top of the stack.
              then go ss xs       -- Match found, continue with the rest of the stack and string.
              else False          -- Mismatched bracket.
      | otherwise = go stack xs -- Ignore non-bracket characters.

    -- | Maps an opening bracket to its corresponding closing bracket.
    openToClose :: Char -> Char
    openToClose '(' = ')'
    openToClose '[' = ']'
    openToClose '{' = '}'
    openToClose c   = c -- Should not happen for valid opening brackets
