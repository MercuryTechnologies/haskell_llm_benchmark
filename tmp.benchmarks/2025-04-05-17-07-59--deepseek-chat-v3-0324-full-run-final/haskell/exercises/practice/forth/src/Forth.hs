{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Data.Text (Text, unpack, toLower, words)
import Data.Map (Map, empty, insert, lookup)
import Data.Char (isDigit)
import Control.Monad (foldM)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState 
  { stack :: [Int]
  , words :: Map Text [Text]
  } deriving (Show)

emptyState :: ForthState
emptyState = ForthState [] empty

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = foldM evalWord state (words text)
  where
    evalWord s word
      | isNumber word = Right $ s { stack = read (unpack word) : stack s }
      | otherwise = case toLower word of
          "+" -> arithmetic s (+)
          "-" -> arithmetic s (-)
          "*" -> arithmetic s (*)
          "/" -> arithmeticSafe s div
          "dup" -> dup s
          "drop" -> dropOp s
          "swap" -> swapOp s
          "over" -> overOp s
          _ -> case unpack word of
            ':' : rest -> defineWord s (words $ toLower (pack rest))
            _ -> applyCustom s word

    isNumber = all isDigit . unpack

arithmetic :: ForthState -> (Int -> Int -> Int) -> Either ForthError ForthState
arithmetic s op = case stack s of
  x:y:xs -> Right $ s { stack = (y `op` x) : xs }
  _ -> Left StackUnderflow

arithmeticSafe :: ForthState -> (Int -> Int -> Int) -> Either ForthError ForthState
arithmeticSafe s op = case stack s of
  x:y:xs -> if x == 0 
            then Left DivisionByZero
            else Right $ s { stack = (y `op` x) : xs }
  _ -> Left StackUnderflow

dup :: ForthState -> Either ForthError ForthState
dup s = case stack s of
  x:xs -> Right $ s { stack = x:x:xs }
  _ -> Left StackUnderflow

dropOp :: ForthState -> Either ForthError ForthState
dropOp s = case stack s of
  _:xs -> Right $ s { stack = xs }
  _ -> Left StackUnderflow

swapOp :: ForthState -> Either ForthError ForthState
swapOp s = case stack s of
  x:y:xs -> Right $ s { stack = y:x:xs }
  _ -> Left StackUnderflow

overOp :: ForthState -> Either ForthError ForthState
overOp s = case stack s of
  x:y:xs -> Right $ s { stack = y:x:y:xs }
  _ -> Left StackUnderflow

defineWord :: ForthState -> [Text] -> Either ForthError ForthState
defineWord s [] = Left InvalidWord
defineWord s (name:rest) = 
  let def = init rest  -- remove trailing semicolon
  in if null def 
     then Left InvalidWord
     else Right $ s { words = insert name def (words s) }

applyCustom :: ForthState -> Text -> Either ForthError ForthState
applyCustom s word = case Data.Map.lookup (toLower word) (words s) of
  Just def -> foldM evalWord s def
  Nothing -> Left (UnknownWord word)

toList :: ForthState -> [Int]
toList = reverse . stack
