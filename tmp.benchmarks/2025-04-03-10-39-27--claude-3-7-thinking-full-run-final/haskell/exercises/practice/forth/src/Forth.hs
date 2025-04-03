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

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int]               -- Stack of integers (top of stack is first element)
  , dictionary :: [(Text, [Text])] -- Dictionary of defined words
  }

emptyState :: ForthState
emptyState = ForthState [] []

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = evalTokens (tokenize text) state

toList :: ForthState -> [Int]
toList = reverse . stack

-- Tokenize input text
tokenize :: Text -> [Text]
tokenize = map (T.map toLower) . T.words

-- Evaluate a list of tokens
evalTokens :: [Text] -> ForthState -> Either ForthError ForthState
evalTokens [] state = Right state
evalTokens (":":rest) state = case parseDefinition rest of
  Nothing -> Left InvalidWord
  Just (name, definition, remaining) ->
    if isNumber name
      then Left InvalidWord
      else evalTokens remaining $ 
           state { dictionary = (name, definition) : dictionary state }
evalTokens (token:rest) state = do
  newState <- evalToken token state
  evalTokens rest newState

-- Parse a word definition
parseDefinition :: [Text] -> Maybe (Text, [Text], [Text])
parseDefinition [] = Nothing
parseDefinition (name:rest) = 
  let (definition, remaining) = break (== ";") rest
  in case remaining of
       (";":xs) -> Just (name, definition, xs)
       _ -> Nothing

-- Evaluate a single token
evalToken :: Text -> ForthState -> Either ForthError ForthState
evalToken token state
  | isNumber token = Right $ state { stack = read (T.unpack token) : stack state }
  | otherwise = case lookup token (dictionary state) of
      -- First check if the word is in the dictionary
      Just definition -> evalTokens definition state
      -- If not, check if it's a built-in word
      Nothing
        | token == "+" -> binaryOp (+) state
        | token == "-" -> binaryOp (-) state
        | token == "*" -> binaryOp (*) state
        | token == "/" -> divideOp state
        | token == "dup" -> dupOp state
        | token == "drop" -> dropOp state
        | token == "swap" -> swapOp state
        | token == "over" -> overOp state
        | otherwise -> Left $ UnknownWord token

-- Check if a token is a number
isNumber :: Text -> Bool
isNumber t = case T.uncons t of
  Just ('-', rest) -> not (T.null rest) && T.all isDigit rest
  _ -> T.all isDigit t

-- Binary operation
binaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binaryOp op state = 
  case stack state of
    (b:a:rest) -> Right $ state { stack = (a `op` b) : rest }
    _ -> Left StackUnderflow

-- Division operation
divideOp :: ForthState -> Either ForthError ForthState
divideOp state =
  case stack state of
    (0:_:_) -> Left DivisionByZero
    (b:a:rest) -> Right $ state { stack = (a `div` b) : rest }
    _ -> Left StackUnderflow

-- DUP operation
dupOp :: ForthState -> Either ForthError ForthState
dupOp state =
  case stack state of
    (a:rest) -> Right $ state { stack = a : a : rest }
    _ -> Left StackUnderflow

-- DROP operation
dropOp :: ForthState -> Either ForthError ForthState
dropOp state =
  case stack state of
    (_:rest) -> Right $ state { stack = rest }
    _ -> Left StackUnderflow

-- SWAP operation
swapOp :: ForthState -> Either ForthError ForthState
swapOp state =
  case stack state of
    (b:a:rest) -> Right $ state { stack = a : b : rest }
    _ -> Left StackUnderflow

-- OVER operation
overOp :: ForthState -> Either ForthError ForthState
overOp state =
  case stack state of
    (b:a:rest) -> Right $ state { stack = a : b : a : rest }
    _ -> Left StackUnderflow
