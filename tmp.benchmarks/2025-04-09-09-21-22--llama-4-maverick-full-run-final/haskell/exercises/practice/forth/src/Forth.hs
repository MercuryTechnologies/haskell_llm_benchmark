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
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (foldl')

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int]
  , definitions :: Map Text [Text]
  } deriving (Show)

emptyState :: ForthState
emptyState = ForthState [] M.empty

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = foldl' step (Right state) (T.words text)
  where
    step (Right s) w = evalWord w s
    step e _ = e

    evalWord :: Text -> ForthState -> Either ForthError ForthState
    evalWord w s
      | T.all (`elem` ['0'..'9']) w = Right $ push (read (T.unpack w)) s
      | w `M.member` definitions s = evalDefinition w s
      | otherwise = case T.toLower w of
          "+" -> binaryOp (+) s
          "-" -> binaryOp (-) s
          "*" -> binaryOp (*) s
          "/" -> binaryOp safeDiv s
          "dup" -> dup s
          "drop" -> dropElem s
          "swap" -> swap s
          "over" -> over s
          ':' : _ -> defineWord w s
          _ -> Left (UnknownWord w)

    push :: Int -> ForthState -> ForthState
    push x (ForthState xs defs) = ForthState (x:xs) defs

    binaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
    binaryOp op (ForthState (y:x:xs) defs) = Right $ ForthState (op x y : xs) defs
    binaryOp _ _ = Left StackUnderflow

    safeDiv :: Int -> Int -> Int
    safeDiv _ 0 = error "Division by zero"
    safeDiv x y = x `div` y

    dup :: ForthState -> Either ForthError ForthState
    dup (ForthState (x:xs) defs) = Right $ ForthState (x:x:xs) defs
    dup _ = Left StackUnderflow

    dropElem :: ForthState -> Either ForthError ForthState
    dropElem (ForthState (_:xs) defs) = Right $ ForthState xs defs
    dropElem _ = Left StackUnderflow

    swap :: ForthState -> Either ForthError ForthState
    swap (ForthState (y:x:xs) defs) = Right $ ForthState (x:y:xs) defs
    swap _ = Left StackUnderflow

    over :: ForthState -> Either ForthError ForthState
    over (ForthState (y:x:xs) defs) = Right $ ForthState (x:y:x:xs) defs
    over _ = Left StackUnderflow

    defineWord :: Text -> ForthState -> Either ForthError ForthState
    defineWord w s = case T.words (T.drop 1 w) of
      (name:defs) -> if T.last defs == ';'
        then let newDefs = M.insert (T.toLower name) (T.words (T.init defs)) (definitions s)
             in Right $ s { definitions = newDefs }
        else Left InvalidWord
      _ -> Left InvalidWord

    evalDefinition :: Text -> ForthState -> Either ForthError ForthState
    evalDefinition w s = foldl' step (Right s) (definitions s M.! w)
      where
        step (Right st) word = evalWord word st
        step e _ = e

toList :: ForthState -> [Int]
toList = reverse . stack
