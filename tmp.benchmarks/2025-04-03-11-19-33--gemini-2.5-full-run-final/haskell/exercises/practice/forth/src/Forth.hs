{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState -- Keep it abstract, don't export constructor
  , evalText
  , toList
  , emptyState
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Char (isDigit)
-- Removed: import Data.List (lookup) -- lookup is in Prelude

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- Store the stack and user definitions (word name mapped to list of words in its definition)
type Definitions = [(Text, [Text])]
type Stack = [Int]
data ForthState = ForthState { stack :: Stack, definitions :: Definitions } deriving (Show, Eq)

-- Initial state: empty stack, no definitions
emptyState :: ForthState
emptyState = ForthState [] []

-- Convert state's stack to a list as required (top of stack is last element)
toList :: ForthState -> [Int]
toList = reverse . stack -- Internal stack has top at the head for efficiency

-- Helper to parse numbers according to the spec (sequence of digits)
parseNumber :: Text -> Maybe Int
parseNumber txt | not (T.null txt) && T.all isDigit txt =
    -- Use decimal reader for robustness
    case TR.decimal txt of
      Right (n, "") -> Just n
      _             -> Nothing -- Should not happen if T.all isDigit passed and not empty
parseNumber _ = Nothing

-- Push a number onto the stack
push :: Int -> ForthState -> Either ForthError ForthState
push n state = Right $ state { stack = n : stack state }

-- Built-in operations map: associates word Text with function
builtInOps :: [(Text, ForthState -> Either ForthError ForthState)]
builtInOps =
  [ ("+", applyBinary (+))
  , ("-", applyBinary (-))
  , ("*", applyBinary (*))
  , ("/", applyDivision)
  , ("dup", applyDup)
  , ("drop", applyDrop)
  , ("swap", applySwap)
  , ("over", applyOver)
  ]

-- Helper for binary operators + - *
-- Takes (Int -> Int -> Int) operation and current state
-- Returns new state or StackUnderflow error
applyBinary :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
applyBinary op state = case stack state of
  -- Pop y, then x, push (op x y)
  (y:x:rest) -> Right $ state { stack = op x y : rest }
  _          -> Left StackUnderflow -- Needs at least two elements

-- Special helper for division /
-- Checks for StackUnderflow and DivisionByZero
applyDivision :: ForthState -> Either ForthError ForthState
applyDivision state = case stack state of
  []        -> Left StackUnderflow -- Needs two elements
  [_]       -> Left StackUnderflow -- Needs two elements
  (0:_:_)   -> Left DivisionByZero -- Check divisor y (top of stack)
  (y:x:rest) -> Right $ state { stack = x `div` y : rest } -- Integer division

-- DUP: ( a -- a a )
applyDup :: ForthState -> Either ForthError ForthState
applyDup state = case stack state of
  (x:rest) -> Right $ state { stack = x : x : rest } -- Duplicate top element
  _        -> Left StackUnderflow -- Needs at least one element

-- DROP: ( a -- )
applyDrop :: ForthState -> Either ForthError ForthState
applyDrop state = case stack state of
  (_:rest) -> Right $ state { stack = rest } -- Discard top element
  _        -> Left StackUnderflow -- Needs at least one element

-- SWAP: ( a b -- b a )
applySwap :: ForthState -> Either ForthError ForthState
applySwap state = case stack state of
  (y:x:rest) -> Right $ state { stack = x : y : rest } -- Swap top two elements
  _          -> Left StackUnderflow -- Needs at least two elements

-- OVER: ( a b -- a b a )
applyOver :: ForthState -> Either ForthError ForthState
applyOver state = case stack state of
  (y:x:rest) -> Right $ state { stack = x : y : x : rest } -- Copy second element to top
  _          -> Left StackUnderflow -- Needs at least two elements

-- Helper to update definitions list
-- Handles redefinition by removing any existing definition for the name before adding the new one
updateDefs :: Definitions -> Text -> [Text] -> Definitions
updateDefs defs name definition = (name, definition) : filter ((/= name) . fst) defs

-- Core evaluation function: processes text input
evalText :: Text -> ForthState -> Either ForthError ForthState
-- First, convert all input to lower case for case-insensitivity, then split into words
evalText text initialState = eval (T.words $ T.toLower text) initialState
  where
    -- Main evaluation loop: processes list of tokens recursively
    eval :: [Text] -> ForthState -> Either ForthError ForthState
    eval [] state = Right state -- Base case: no more tokens, return current state successfully
    eval (token:ts) state -- Process the next token `token`, `ts` are remaining tokens

      -- Handle definition start ":"
      | token == ":" =
          -- Call parseDefinition, which consumes tokens until ";"
          -- It returns the new state (with the definition added) and the remaining tokens *after* ";"
          parseDefinition ts state >>= \(newState, remainingTokens) ->
          -- Continue evaluation with the remaining tokens and the updated state
          eval remainingTokens newState

      -- Handle numbers: try parsing the token as a number
      | Just num <- parseNumber token =
          -- If successful, push the number onto the stack
          push num state >>=
          -- Continue evaluation with the rest of the tokens
          eval ts

      -- *** Check user-defined words FIRST ***
      | Just definition <- lookup token (definitions state) =
          -- If found, evaluate the list of words in the definition *first*
          -- Pass the *current* state to this recursive call
          eval definition state >>=
          -- Then, once the definition's words are evaluated, continue evaluating
          -- the rest of the original tokens (`ts`) using the state *resulting* from the definition's evaluation
          eval ts

      -- *** Check built-in words SECOND ***
      | Just op <- lookup token builtInOps =
          -- If found, apply the associated operation function to the state
          op state >>=
          -- Continue evaluation with the rest of the tokens
          eval ts

      -- Handle unknown words: if token is not ":", a number, user-defined, or built-in
      | otherwise = Left (UnknownWord token)

    -- Parses a definition (: word ... ;)
    -- Takes the tokens *after* ":" and the current state
    -- Returns the updated state (with the new definition) and the list of tokens *after* ";"
    -- Or returns InvalidWord error
    parseDefinition :: [Text] -> ForthState -> Either ForthError (ForthState, [Text])
    parseDefinition [] _ = Left InvalidWord -- Error: Input ended unexpectedly after ":"
    parseDefinition (wordName : restTokens) state
      -- Rule: Cannot redefine numbers
      | parseNumber wordName /= Nothing = Left InvalidWord
      -- Rule: Word name cannot be empty (shouldn't happen with T.words, but good practice)
      | T.null wordName = Left InvalidWord
      | otherwise =
          -- Find the end of the definition, marked by ";"
          -- `break` splits the list at the first occurrence of ";"
          -- `defTokens` = words before ";", `afterSemi` = words from ";" onwards
          case break (== ";") restTokens of
            (_, []) -> Left InvalidWord -- Error: No closing ";" found
            (defTokens, _semicolon : remainingTokens) -> -- Success: Found definition and remaining tokens
              -- Add/update the definition in the state
              let newDefs = updateDefs (definitions state) wordName defTokens
                  newState = state { definitions = newDefs }
              -- Return the new state and the tokens remaining after the definition
              in Right (newState, remainingTokens)

