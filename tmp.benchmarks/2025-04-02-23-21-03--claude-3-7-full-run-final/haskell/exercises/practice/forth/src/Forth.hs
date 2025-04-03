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
import qualified Data.Map.Strict as M
import Data.Char (isDigit, toLower)
import Control.Monad (foldM)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int]
  , dictionary :: M.Map Text [Text]
  } deriving (Show)

emptyState :: ForthState
emptyState = ForthState [] M.empty

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = foldM evalToken state (tokenize text)

toList :: ForthState -> [Int]
toList = stack

-- Tokenize the input text
tokenize :: Text -> [Text]
tokenize = filter (not . T.null) . T.split (== ' ') . T.map replaceNewlines
  where replaceNewlines c = if c == '\n' then ' ' else c

-- Evaluate a single token
evalToken :: ForthState -> Text -> Either ForthError ForthState
evalToken state token
  | isDefiningWord token = parseDefinition state (tokenize token)
  | isNumber token = Right $ pushStack (read $ T.unpack token) state
  | otherwise = evalWord (T.toLower token) state

-- Check if a token is a number
isNumber :: Text -> Bool
isNumber text = case T.uncons text of
  Just ('-', rest) -> not (T.null rest) && T.all isDigit rest
  _ -> T.all isDigit text

-- Check if a token is a word definition
isDefiningWord :: Text -> Bool
isDefiningWord text = ":" `T.isPrefixOf` text && ";" `T.isSuffixOf` text

-- Parse a word definition
parseDefinition :: ForthState -> [Text] -> Either ForthError ForthState
parseDefinition state tokens = 
  case tokens of
    (":" : name : body) -> 
      if T.null name || isNumber name || name == ":" || name == ";"
        then Left InvalidWord
        else Right $ defineWord (T.toLower name) (init body) state
    _ -> Left InvalidWord

-- Define a new word in the dictionary
defineWord :: Text -> [Text] -> ForthState -> ForthState
defineWord name definition state = 
  state { dictionary = M.insert name definition (dictionary state) }

-- Evaluate a word
evalWord :: Text -> ForthState -> Either ForthError ForthState
evalWord word state
  | word == "+" = binaryOp (+) state
  | word == "-" = binaryOp (-) state
  | word == "*" = binaryOp (*) state
  | word == "/" = divOp state
  | word == "dup" = dupOp state
  | word == "drop" = dropOp state
  | word == "swap" = swapOp state
  | word == "over" = overOp state
  | otherwise = case M.lookup word (dictionary state) of
      Just definition -> foldM evalToken state definition
      Nothing -> Left $ UnknownWord word

-- Push a value onto the stack
pushStack :: Int -> ForthState -> ForthState
pushStack val state = state { stack = val : stack state }

-- Pop a value from the stack
popStack :: ForthState -> Either ForthError (Int, ForthState)
popStack state = case stack state of
  [] -> Left StackUnderflow
  (x:xs) -> Right (x, state { stack = xs })

-- Binary operation helper
binaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binaryOp op state = do
  (b, state1) <- popStack state
  (a, state2) <- popStack state1
  Right $ pushStack (a `op` b) state2

-- Division operation
divOp :: ForthState -> Either ForthError ForthState
divOp state = do
  (b, state1) <- popStack state
  if b == 0
    then Left DivisionByZero
    else do
      (a, state2) <- popStack state1
      Right $ pushStack (a `div` b) state2

-- DUP operation
dupOp :: ForthState -> Either ForthError ForthState
dupOp state = do
  (a, _) <- popStack state
  Right $ pushStack a state

-- DROP operation
dropOp :: ForthState -> Either ForthError ForthState
dropOp state = do
  (_, state1) <- popStack state
  Right state1

-- SWAP operation
swapOp :: ForthState -> Either ForthError ForthState
swapOp state = do
  (b, state1) <- popStack state
  (a, state2) <- popStack state1
  Right $ pushStack a $ pushStack b state2

-- OVER operation
overOp :: ForthState -> Either ForthError ForthState
overOp state = do
  (b, state1) <- popStack state
  (a, state2) <- popStack state1
  Right $ pushStack a $ pushStack b $ pushStack a state2
