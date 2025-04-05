{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Data.Char (isDigit, toLower)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int]
  , userDefs :: Map.Map Text [Text]
  } deriving (Show, Eq)

emptyState :: ForthState
emptyState = ForthState [] Map.empty

toList :: ForthState -> [Int]
toList = reverse . stack

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText input state = evalTokens (tokenize input) state

tokenize :: Text -> [Text]
tokenize = T.words . T.toLower

evalTokens :: [Text] -> ForthState -> Either ForthError ForthState
evalTokens [] st = Right st
evalTokens (":" : rest) st = defineWord rest st
evalTokens (tok : rest) st = do
  st' <- evalToken tok st
  evalTokens rest st'

defineWord :: [Text] -> ForthState -> Either ForthError ForthState
defineWord tokens st =
  case break (== ";") tokens of
    ([], _) -> Left InvalidWord
    (_, []) -> Left InvalidWord
    (name:definition, (_:rest)) ->
      if isNumber name
        then Left InvalidWord
        else evalTokens rest st { userDefs = Map.insert name definition (userDefs st) }

evalToken :: Text -> ForthState -> Either ForthError ForthState
evalToken tok st
  | isNumber tok = Right st { stack = (readInt tok) : stack st }
  | otherwise =
      case tok of
        "+" -> binaryOp st (+)
        "-" -> binaryOp st (-)
        "*" -> binaryOp st (*)
        "/" -> divOp st
        "dup" -> dupOp st
        "drop" -> dropOp st
        "swap" -> swapOp st
        "over" -> overOp st
        _ -> case Map.lookup tok (userDefs st) of
               Just def -> evalTokens def st
               Nothing -> Left (UnknownWord tok)

isNumber :: Text -> Bool
isNumber t =
  case T.uncons t of
    Just ('-', rest) -> T.all isDigit rest && not (T.null rest)
    _ -> T.all isDigit t && not (T.null t)

readInt :: Text -> Int
readInt = read . T.unpack

binaryOp :: ForthState -> (Int -> Int -> Int) -> Either ForthError ForthState
binaryOp st op =
  case stack st of
    (x:y:xs) -> Right st { stack = (y `op` x) : xs }
    _ -> Left StackUnderflow

divOp :: ForthState -> Either ForthError ForthState
divOp st =
  case stack st of
    (0:_:_) -> Left DivisionByZero
    (x:y:xs) -> Right st { stack = (y `div` x) : xs }
    _ -> Left StackUnderflow

dupOp :: ForthState -> Either ForthError ForthState
dupOp st =
  case stack st of
    (x:xs) -> Right st { stack = x:x:xs }
    _ -> Left StackUnderflow

dropOp :: ForthState -> Either ForthError ForthState
dropOp st =
  case stack st of
    (_:xs) -> Right st { stack = xs }
    _ -> Left StackUnderflow

swapOp :: ForthState -> Either ForthError ForthState
swapOp st =
  case stack st of
    (x:y:xs) -> Right st { stack = y:x:xs }
    _ -> Left StackUnderflow

overOp :: ForthState -> Either ForthError ForthState
overOp st =
  case stack st of
    (x:y:xs) -> Right st { stack = y:x:y:xs }
    _ -> Left StackUnderflow
