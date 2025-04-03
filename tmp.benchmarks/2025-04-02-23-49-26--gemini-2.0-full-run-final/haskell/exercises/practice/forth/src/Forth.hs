{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Data.Text (Text, pack, unpack, splitOn)
import qualified Data.Text as T
import Data.Char (isDigit)
import Data.Map (Map, empty, insert, lookup)
import qualified Data.Map as M

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

type Stack = [Int]

data ForthState = ForthState {
  stack :: Stack,
  dictionary :: Map Text [Text]
}

emptyState :: ForthState
emptyState = ForthState { stack = [], dictionary = empty }

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText input state = foldl evalWord (Right state) (splitOn (pack " ") input)

evalWord :: Either ForthError ForthState -> Text -> Either ForthError ForthState
evalWord (Left err) _ = Left err
evalWord (Right state) word =
  case unpack word of
    ":" -> Right $ state { dictionary = M.insert (pack $ head $ tail $ words') (map pack $ tail $ tail $ words' ) (dictionary state)}
         where words' = words $ unpack $ input'
               input' = T.replace (pack ";") (pack "") word
    ";" -> Right state
    "+" -> applyBinary (+) state
    "-" -> applyBinary (-) state
    "*" -> applyBinary (*) state
    "/" -> applyBinary div' state
    "DUP" -> applyDup state
    "DROP" -> applyDrop state
    "SWAP" -> applySwap state
    "OVER" -> applyOver state
    _ -> case M.lookup word (dictionary state) of
           Just words' -> foldl evalWord (Right state) words'
           Nothing -> if all isDigit (unpack word)
                        then push (read (unpack word)) state
                        else Left $ UnknownWord word

  where
    applyBinary :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
    applyBinary op s = case stack s of
                         (x:y:rest) -> push (op y x) (s { stack = rest })
                         _ -> Left StackUnderflow

    div' :: Int -> Int -> Int
    div' _ 0 = error "divide by zero"
    div' x y = x `div` y

    applyDup :: ForthState -> Either ForthError ForthState
    applyDup s = case stack s of
                   (x:rest) -> push x (s { stack = x:rest })
                   _ -> Left StackUnderflow

    applyDrop :: ForthState -> Either ForthError ForthState
    applyDrop s = case stack s of
                    (_:rest) -> Right (s { stack = rest })
                    _ -> Left StackUnderflow

    applySwap :: ForthState -> Either ForthError ForthState
    applySwap s = case stack s of
                    (x:y:rest) -> Right (s { stack = y:x:rest })
                    _ -> Left StackUnderflow

    applyOver :: ForthState -> Either ForthError ForthState
    applyOver s = case stack s of
                    (x:y:rest) -> push y (s { stack = x:y:rest })
                    _ -> Left StackUnderflow

    push :: Int -> ForthState -> Either ForthError ForthState
    push x s = Right (s { stack = x : stack s })

toList :: ForthState -> [Int]
toList state = reverse $ stack state
