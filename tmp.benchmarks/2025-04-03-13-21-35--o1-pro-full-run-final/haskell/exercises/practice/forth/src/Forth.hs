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

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- A simple representation of Forth's state,
-- storing the stack and any user-defined words.
data ForthState = ForthState
  { stack       :: [Int]
  , definitions :: M.Map Text [Text]
  }
  deriving (Show, Eq)

-- The initial empty state: no stack elements, no definitions.
emptyState :: ForthState
emptyState = ForthState [] M.empty

-- Convert the stack to a list of Ints.
-- The rightmost (last) element is the top of the stack.
toList :: ForthState -> [Int]
toList = stack

-- The main evaluator for Forth code.
-- Takes the input Text, and the current state, and returns
-- an updated state or an error.
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text st = runTokens (tokenize text) st

-------------------------------------------------------------------------
-- Tokenization
-------------------------------------------------------------------------

-- Splits the input text by whitespace into tokens.
tokenize :: Text -> [Text]
tokenize = filter (not . T.null) . T.words

-------------------------------------------------------------------------
-- Main token evaluator
-------------------------------------------------------------------------

-- Process a list of tokens in sequence, threading through ForthState.
runTokens :: [Text] -> ForthState -> Either ForthError ForthState
runTokens [] st = Right st
runTokens (t:ts) st =
  case T.toUpper t of
    ":" -> defineWord ts st        -- begin a definition
    _   -> executeToken t ts st    -- execute or resolve a single token

-------------------------------------------------------------------------
-- Word definition handling
-------------------------------------------------------------------------

defineWord :: [Text] -> ForthState -> Either ForthError ForthState
defineWord [] _ = Left InvalidWord
defineWord (rawName:rest) st =
  let name = T.toUpper rawName
      -- We can't define a numeric word name
      inNum = parseNumber rawName
  in case inNum of
       Just _  -> Left InvalidWord
       Nothing ->
         -- Find the tokens until we get ";"
         case break (== ";") rest of
           (_, []) -> Left InvalidWord  -- no closing ;
           (defTokens, (_:postDef)) ->
             -- Store the definition in uppercase keys
             let newDefs = M.insert name (map T.toUpper <$> defTokens) (definitions st)
                 st' = st { definitions = newDefs }
             in runTokens postDef st'

-------------------------------------------------------------------------
-- Execute a single token
-------------------------------------------------------------------------

executeToken :: Text -> [Text] -> ForthState -> Either ForthError ForthState
executeToken token ts st =
  case parseNumber token of
    Just n  -> -- It's a number, push it
               runTokens ts (st { stack = stack st ++ [n] })
    Nothing ->
      let tU = T.toUpper token
      in case tU of
           -- Built-in words
           "+"    -> doOp2 ts st (\x y -> x + y)
           "-"    -> doOp2 ts st (\x y -> x - y)
           "*"    -> doOp2 ts st (\x y -> x * y)
           "/"    -> doSafeDiv ts st
           "DUP"  -> doDup ts st
           "DROP" -> doDrop ts st
           "SWAP" -> doSwap ts st
           "OVER" -> doOver ts st
           -- Not recognized as a built-in, check definitions
           _      -> expandAndRun tU ts st

-- If the token is user-defined, expand it, otherwise throw UnknownWord
expandAndRun :: Text -> [Text] -> ForthState -> Either ForthError ForthState
expandAndRun tU ts st =
  case M.lookup tU (definitions st) of
    Just body -> runTokens (body ++ ts) st
    Nothing   -> Left (UnknownWord tU)

-------------------------------------------------------------------------
-- Helpers for stack operations
-------------------------------------------------------------------------

-- Pop at least 2 items from the stack (throw error if underflow),
-- apply a function, push result
doOp2 :: [Text] -> ForthState -> (Int -> Int -> Int) -> Either ForthError ForthState
doOp2 ts st f =
  case stack st of
    []       -> Left StackUnderflow
    [_]      -> Left StackUnderflow
    xs       ->
      let (initStack, top2) = splitAt (length xs - 2) xs
          [x1, x2] = top2
          result    = f x1 x2
          newStack  = initStack ++ [result]
      in runTokens ts (st { stack = newStack })

-- Division is special-cased for zero checks
doSafeDiv :: [Text] -> ForthState -> Either ForthError ForthState
doSafeDiv ts st =
  case stack st of
    []       -> Left StackUnderflow
    [_]      -> Left StackUnderflow
    xs       ->
      let (initStack, top2) = splitAt (length xs - 2) xs
          [x1, x2] = top2
      in if x2 == 0
         then Left DivisionByZero
         else runTokens ts (st { stack = initStack ++ [x1 `div` x2] })

-- DUP, DROP, SWAP, OVER
doDup :: [Text] -> ForthState -> Either ForthError ForthState
doDup ts st =
  case stack st of
    []   -> Left StackUnderflow
    xs   ->
      let topVal = last xs
      in runTokens ts (st { stack = xs ++ [topVal] })

doDrop :: [Text] -> ForthState -> Either ForthError ForthState
doDrop ts st =
  case stack st of
    []   -> Left StackUnderflow
    xs   ->
      let newStack = init xs
      in runTokens ts (st { stack = newStack })

doSwap :: [Text] -> ForthState -> Either ForthError ForthState
doSwap ts st =
  case stack st of
    []       -> Left StackUnderflow
    [_]      -> Left StackUnderflow
    xs       ->
      let front = take (length xs - 2) xs
          [a, b] = drop (length xs - 2) xs
      in runTokens ts (st { stack = front ++ [b, a] })

doOver :: [Text] -> ForthState -> Either ForthError ForthState
doOver ts st =
  case stack st of
    []       -> Left StackUnderflow
    [_]      -> Left StackUnderflow
    xs       ->
      let secondFromTop = xs !! (length xs - 2)
      in runTokens ts (st { stack = xs ++ [secondFromTop] })

-------------------------------------------------------------------------
-- Parsing utilities
-------------------------------------------------------------------------

-- Attempt to parse an integer from Text. Return Just Int if it succeeds,
-- otherwise Nothing.
parseNumber :: Text -> Maybe Int
parseNumber t =
  if T.all C.isDigit t || (T.head t == '-' && T.all C.isDigit (T.tail t))
    then case reads (T.unpack t) of
           [(n, "")] -> Just n
           _         -> Nothing
    else Nothing
