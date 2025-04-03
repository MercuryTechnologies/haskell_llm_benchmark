{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Data.Text (Text, pack, unpack, words)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.State

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int]
  , words :: Map.Map Text [Text]
  }

emptyState :: ForthState
emptyState = ForthState [] Map.empty

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = 
  let tokens = map (pack . unpack) (words text)
  in foldM evalToken state tokens

evalToken :: ForthState -> Text -> Either ForthError ForthState
evalToken state token
  | token == "+" = binaryOp (+) state
  | token == "-" = binaryOp (-) state
  | token == "*" = binaryOp (*) state
  | token == "/" = binaryOp div state
  | token == "DUP" = dup state
  | token == "DROP" = drop state
  | token == "SWAP" = swap state
  | token == "OVER" = over state
  | otherwise = case reads (unpack token) :: [(Int, String)] of
      [(n, "")] -> Right $ push n state
      _ -> case Map.lookup token (words state) of
        Just def -> foldM evalToken state def
        Nothing -> Left $ UnknownWord token

binaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binaryOp op (ForthState (x:y:xs) w) = Right $ ForthState ((y `op` x) : xs) w
binaryOp _ _ = Left StackUnderflow

dup :: ForthState -> Either ForthError ForthState
dup (ForthState (x:xs) w) = Right $ ForthState (x:x:xs) w
dup _ = Left StackUnderflow

drop :: ForthState -> Either ForthError ForthState
drop (ForthState (_:xs) w) = Right $ ForthState xs w
drop _ = Left StackUnderflow

swap :: ForthState -> Either ForthError ForthState
swap (ForthState (x:y:xs) w) = Right $ ForthState (y:x:xs) w
swap _ = Left StackUnderflow

over :: ForthState -> Either ForthError ForthState
over (ForthState (x:y:xs) w) = Right $ ForthState (y:x:y:xs) w
over _ = Left StackUnderflow

push :: Int -> ForthState -> ForthState
push n (ForthState xs w) = ForthState (n:xs) w

toList :: ForthState -> [Int]
toList (ForthState s _) = reverse s
