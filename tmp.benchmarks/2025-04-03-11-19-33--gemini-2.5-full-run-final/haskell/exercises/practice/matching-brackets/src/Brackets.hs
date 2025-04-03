module Brackets (arePaired) where

import Data.Maybe (isJust, fromMaybe)

-- | Determines if the brackets in a string are correctly paired and nested.
arePaired :: String -> Bool
arePaired xs = case foldl step (Just []) xs of
                 Just [] -> True  -- Successfully processed and stack is empty
                 _       -> False -- Either failed during processing or stack not empty at the end
  where
    -- | Processes one character, updating the stack state.
    --   Nothing represents an invalid state (mismatched brackets).
    --   Just stack represents the current valid stack of opening brackets.
    step :: Maybe [Char] -> Char -> Maybe [Char]
    step Nothing _ = Nothing -- If already invalid, stay invalid
    step (Just stack) c
      | c `elem` "([{" = Just (c : stack) -- Push opening bracket
      | c == ')' = checkMatching '(' stack
      | c == ']' = checkMatching '[' stack
      | c == '}' = checkMatching '{' stack
      | otherwise = Just stack -- Ignore other characters

    -- | Helper to check for matching closing bracket.
    checkMatching :: Char -> [Char] -> Maybe [Char]
    checkMatching openChar stack =
      case stack of
        (top:rest) | top == openChar -> Just rest -- Match found, pop stack
        _                            -> Nothing   -- No match or empty stack, invalid
