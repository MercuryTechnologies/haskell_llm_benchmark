{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isDigit, toLower)
import qualified Data.Map.Strict as Map

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int]           -- head is the top of stack
  , defs  :: Map.Map String [String]
  } deriving (Show, Eq)

emptyState :: ForthState
emptyState = ForthState [] Map.empty

-- Returns the internal stack with the bottom element first and the top element last.
toList :: ForthState -> [Int]
toList st = reverse (stack st)

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText input st = evalTokens (map T.unpack (T.words input)) st

-- Helper: determine if a token is a valid number representation.
isNumber :: String -> Bool
isNumber s = case reads s :: [(Int, String)] of
  [(n, "")] -> True
  _         -> False

-- Evaluates a list of tokens.
evalTokens :: [String] -> ForthState -> Either ForthError ForthState
evalTokens [] st = Right st
evalTokens (token:tokens) st
  | token == ":" = defineWord tokens st
  | otherwise =
      let lowered = map toLower token in
      if isNumber token then 
         let n = read token :: Int in
         evalTokens tokens st { stack = n : stack st }
      else if lowered `elem` ["+", "-", "*", "/"] then
         performArithmetic lowered st >>= \st' -> evalTokens tokens st'
      else if lowered `elem` ["dup", "drop", "swap", "over"] then
         performStackOp lowered st >>= \st' -> evalTokens tokens st'
      else case Map.lookup lowered (defs st) of
             Just definition ->
               -- Evaluate the definition's tokens, then continue with remaining tokens.
               evalTokens definition st >>= \st' -> evalTokens tokens st'
             Nothing -> Left (UnknownWord (T.pack token))

-- Handles word definitions.
defineWord :: [String] -> ForthState -> Either ForthError ForthState
defineWord [] _ = Left InvalidWord
defineWord (name:rest) st
  | isNumber name = Left InvalidWord
  | otherwise =
      let nameLower = map toLower name in
      let (definition, restTokens) = span (/= ";") rest in
      case restTokens of
         [] -> Left InvalidWord  -- Missing terminating ';'
         (_:remaining) ->
             let defTokens = map (map toLower) definition in
             let newDefs = Map.insert nameLower defTokens (defs st) in
             evalTokens remaining st { defs = newDefs }

-- Performs arithmetic operations.
performArithmetic :: String -> ForthState -> Either ForthError ForthState
performArithmetic op st =
  case stack st of
    (x:y:rest) ->
      case op of
        "+" -> Right st { stack = (y + x) : rest }
        "-" -> Right st { stack = (y - x) : rest }
        "*" -> Right st { stack = (y * x) : rest }
        "/" -> if x == 0 then Left DivisionByZero else Right st { stack = (y `div` x) : rest }
        _   -> Left (UnknownWord (T.pack op))
    _ -> Left StackUnderflow

-- Performs stack manipulation operations.
performStackOp :: String -> ForthState -> Either ForthError ForthState
performStackOp op st =
  case op of
    "dup" ->
      case stack st of
        (x:xs) -> Right st { stack = x : x : xs }
        _ -> Left StackUnderflow
    "drop" ->
      case stack st of
        (_:xs) -> Right st { stack = xs }
        _ -> Left StackUnderflow
    "swap" ->
      case stack st of
        (x:y:xs) -> Right st { stack = y : x : xs }
        _ -> Left StackUnderflow
    "over" ->
      case stack st of
        (x:y:xs) -> Right st { stack = y : x : y : xs }
        _ -> Left StackUnderflow
    _ -> Left (UnknownWord (T.pack op))
