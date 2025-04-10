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
import qualified Data.Char as C
import qualified Data.Map.Strict as M
import Data.List (foldl')
import Data.Bifunctor (first)
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- ForthState: stack and dictionary of user-defined words
data ForthState = ForthState
  { stack :: [Int]
  , dict  :: M.Map Text [Token]
  } deriving (Show, Eq)

emptyState :: ForthState
emptyState = ForthState [] M.empty

-- Token: either a number or a word
data Token = Number Int | Word Text deriving (Show, Eq)

-- Helper: parse a single token
parseToken :: Text -> Token
parseToken t =
  case T.signed T.decimal t of
    Right (n, "") -> Number n
    _             -> Word (T.toCaseFold t)

-- Helper: split input into tokens
tokenize :: Text -> [Token]
tokenize = map parseToken . T.words

-- Helper: check if a Text is a number
isNumber :: Text -> Bool
isNumber t =
  case T.signed T.decimal t of
    Right (_, "") -> True
    _             -> False

-- Evaluate a list of tokens
evalTokens :: [Token] -> ForthState -> Either ForthError ForthState
evalTokens = foldM step
  where
    step :: ForthState -> Token -> Either ForthError ForthState
    step st (Number n) = pure st { stack = stack st ++ [n] }
    step st (Word w)
      | w == "+"    = binOp st (+)
      | w == "-"    = binOp st (-)
      | w == "*"    = binOp st (*)
      | w == "/"    = divOp st
      | w == "dup"  = dupOp st
      | w == "drop" = dropOp st
      | w == "swap" = swapOp st
      | w == "over" = overOp st
      | otherwise   =
          case M.lookup w (dict st) of
            Just ts -> evalTokens ts st
            Nothing -> Left (UnknownWord w)

    binOp st f =
      case stack st of
        xs@(_:_:_) ->
          let (a:b:rest) = reverse xs
          in pure st { stack = reverse (f b a : rest) }
        _ -> Left StackUnderflow

    divOp st =
      case stack st of
        xs@(_:_:_) ->
          let (a:b:rest) = reverse xs
          in if a == 0
             then Left DivisionByZero
             else pure st { stack = reverse (b `div` a : rest) }
        _ -> Left StackUnderflow

    dupOp st =
      case stack st of
        [] -> Left StackUnderflow
        xs -> pure st { stack = xs ++ [last xs] }

    dropOp st =
      case stack st of
        [] -> Left StackUnderflow
        xs -> pure st { stack = init xs }

    swapOp st =
      case stack st of
        xs@(_:_:_) ->
          let (a:b:rest) = reverse xs
          in pure st { stack = reverse (b:a:rest) }
        _ -> Left StackUnderflow

    overOp st =
      case stack st of
        xs@(_:_:_) ->
          let (a:b:rest) = reverse xs
          in pure st { stack = reverse (b:a:b:rest) }
        _ -> Left StackUnderflow

-- Parse and handle word definitions, then evaluate
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText input st0 = evalLines (T.lines input) st0
  where
    evalLines [] st = Right st
    evalLines (l:ls) st =
      case parseDefinition l of
        Just (name, def) ->
          if isNumber name
            then Left InvalidWord
            else evalLines ls st { dict = M.insert (T.toCaseFold name) def (dict st) }
        Nothing -> do
          st' <- evalTokens (tokenize l) st
          evalLines ls st'

    -- Parse a definition of the form ": word-name ... ;"
    parseDefinition t =
      let ws = T.words t
      in if not (null ws) && head ws == ":" && last ws == ";" && length ws >= 3
         then let name = ws !! 1
                  def  = map parseToken (take (length ws - 3) (drop 2 ws))
              in Just (name, def)
         else Nothing

toList :: ForthState -> [Int]
toList = stack
