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
import Data.Char (isDigit, isSpace)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int]
  , definitions :: Map Text [Text]
  } deriving (Show, Eq)

emptyState :: ForthState
emptyState = ForthState [] Map.empty

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = foldl' evalWord (Right state) (T.words $ T.toLower text)

evalWord :: Either ForthError ForthState -> Text -> Either ForthError ForthState
evalWord (Left err) _ = Left err
evalWord (Right state) word
  | T.all isDigit word = Right $ state { stack = (read $ T.unpack word) : stack state }
  | word == "+" = arithmeticOp (+) state
  | word == "-" = arithmeticOp (-) state
  | word == "*" = arithmeticOp (*) state
  | word == "/" = if 0 `elem` take 1 (stack state) then Left DivisionByZero else arithmeticOp div state
  | word == "dup" = stackOp dup state
  | word == "drop" = stackOp dropOp state
  | word == "swap" = stackOp swap state
  | word == "over" = stackOp over state
  | word == ":" = Left InvalidWord -- ':' should be handled separately
  | otherwise = case Map.lookup word (definitions state) of
      Just def -> foldl' evalWord (Right state) def
      Nothing -> Left $ UnknownWord word

arithmeticOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
arithmeticOp op state
  | length (stack state) < 2 = Left StackUnderflow
  | otherwise = Right $ state { stack = (x `op` y) : rest }
  where (x:y:rest) = stack state

stackOp :: ([Int] -> Either ForthError [Int]) -> ForthState -> Either ForthError ForthState
stackOp op state = case op (stack state) of
  Left err -> Left err
  Right newStack -> Right $ state { stack = newStack }

dup, dropOp, swap, over :: [Int] -> Either ForthError [Int]
dup (x:xs) = Right (x:x:xs)
dup _ = Left StackUnderflow

dropOp (_:xs) = Right xs
dropOp _ = Left StackUnderflow

swap (x:y:xs) = Right (y:x:xs)
swap _ = Left StackUnderflow

over (x:y:xs) = Right (y:x:y:xs)
over _ = Left StackUnderflow

toList :: ForthState -> [Int]
toList = reverse . stack
