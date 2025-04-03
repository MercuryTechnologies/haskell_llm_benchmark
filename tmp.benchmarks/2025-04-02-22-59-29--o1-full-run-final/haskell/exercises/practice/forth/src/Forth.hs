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

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- Each user-defined word is stored as a list of 'Op',
-- which can be a 'Push' of an integer or a 'BuiltIn' operation.
-- We expand user-defined words *at definition time*, so each dictionary entry
-- stores only built-in ops and literal pushes, never references to other words.
data Op
  = Push Int
  | BuiltIn Text
  deriving (Show, Eq)

-- We'll keep the stack in a list (top at head),
-- plus a simple association list for word definitions, each mapped to a list of Op.
data ForthState = FS
  { stack :: [Int]
  , dict  :: [(Text, [Op])]
  } deriving (Show, Eq)

emptyState :: ForthState
emptyState = FS [] []

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText input st = interpretAll st (tokenize input)

toList :: ForthState -> [Int]
toList (FS stk _) = reverse stk  -- rightmost is top of stack

-- Split the input into uppercase tokens.
tokenize :: Text -> [Text]
tokenize txt = map (T.toUpper . T.strip) (T.words txt)

----------------------------------------------------------------------------
-- Top-level interpretation
----------------------------------------------------------------------------

interpretAll :: ForthState -> [Text] -> Either ForthError ForthState
interpretAll st [] = Right st
interpretAll st (t:ts) =
  case t of
    ":" -> defineWord st ts
    _   -> executeToken st t ts

----------------------------------------------------------------------------
-- Word definition
----------------------------------------------------------------------------

-- Parse and store a new Forth word definition, e.g.
--   : SWAP DUP ;
-- The definition is everything up to ";".
defineWord :: ForthState -> [Text] -> Either ForthError ForthState
defineWord _ [] = Left InvalidWord
defineWord st (w:ws)
  | isNumber w = Left InvalidWord
  | otherwise =
      let (bodyTokens, rest) = break (== ";") ws
      in case rest of
           [] -> Left InvalidWord  -- Missing semicolon
           (_ : remaining) ->
             let wordName = T.toUpper w
             in case compileDefinition st bodyTokens of
                  Left err -> Left err
                  Right ops ->
                    -- Store the newly compiled definition, overriding old definitions
                    let newDict = (wordName, ops) : filter ((/= wordName) . fst) (dict st)
                        st'    = FS (stack st) newDict
                    in interpretAll st' remaining

----------------------------------------------------------------------------
-- Compiling a word definition
--
-- Turn tokens (numbers, builtins, already-defined words) into a flat list of Ops.
-- If a token is a user-defined word, we inline its Ops at compile time
-- so that older definitions remain unaffected by later redefinitions.
----------------------------------------------------------------------------

compileDefinition :: ForthState -> [Text] -> Either ForthError [Op]
compileDefinition _ [] = Right []
compileDefinition st (t:ts)
  | isNumber t = do
      let n = readNumber t
      restOps <- compileDefinition st ts
      return (Push n : restOps)
  | isBuiltIn t =
      do restOps <- compileDefinition st ts
         return (BuiltIn t : restOps)
  | otherwise =
      -- It's presumably a user-defined word, so look it up and inline its ops
      case lookup t (dict st) of
        Nothing -> Left (UnknownWord t)
        Just opsForWord -> do
          -- Inline that word's ops, then compile the rest
          restOps <- compileDefinition st ts
          return (opsForWord ++ restOps)

isBuiltIn :: Text -> Bool
isBuiltIn t =
  t `elem` [ "+", "-", "*", "/", "DUP", "DROP", "SWAP", "OVER" ]

----------------------------------------------------------------------------
-- Executing a single token
----------------------------------------------------------------------------

executeToken :: ForthState -> Text -> [Text] -> Either ForthError ForthState
executeToken st t ts
  | isNumber t =
      let n = readNumber t
      in interpretAll (FS (n : stack st) (dict st)) ts
  | isBuiltIn t = do
      st' <- builtinOp t st
      interpretAll st' ts
  | otherwise =
      -- It's presumably a user-defined word; run its already-compiled ops
      case lookup t (dict st) of
        Nothing -> Left (UnknownWord t)
        Just ops -> do
          st' <- runOps st ops
          interpretAll st' ts

----------------------------------------------------------------------------
-- Running a list of compiled ops
----------------------------------------------------------------------------

runOps :: ForthState -> [Op] -> Either ForthError ForthState
runOps st [] = Right st
runOps st (op : more) =
  case op of
    Push x -> runOps (FS (x : stack st) (dict st)) more
    BuiltIn name -> do
      st' <- builtinOp name st
      runOps st' more

----------------------------------------------------------------------------
-- Built-in word dispatch
----------------------------------------------------------------------------

builtinOp :: Text -> ForthState -> Either ForthError ForthState
builtinOp name st =
  case name of
    "+"    -> doBinOp st (+)
    "-"    -> doBinOp st (-)
    "*"    -> doBinOp st (*)
    "/"    -> doDivOp st
    "DUP"  -> doDup st
    "DROP" -> doDrop st
    "SWAP" -> doSwap st
    "OVER" -> doOver st
    _      -> Left (UnknownWord name)

----------------------------------------------------------------------------
-- Helpers for built-in operations
----------------------------------------------------------------------------

doBinOp :: ForthState -> (Int -> Int -> Int) -> Either ForthError ForthState
doBinOp (FS (x:y:xs) d) op = Right (FS (op y x : xs) d)
doBinOp _ _ = Left StackUnderflow

doDivOp :: ForthState -> Either ForthError ForthState
doDivOp (FS (0:_:_) _) = Left DivisionByZero
doDivOp (FS (x:y:xs) d) = Right (FS (y `div` x : xs) d)
doDivOp _ = Left StackUnderflow

doDup :: ForthState -> Either ForthError ForthState
doDup (FS [] _) = Left StackUnderflow
doDup (FS (x:xs) d) = Right (FS (x:x:xs) d)

doDrop :: ForthState -> Either ForthError ForthState
doDrop (FS [] _) = Left StackUnderflow
doDrop (FS (_:xs) d) = Right (FS xs d)

doSwap :: ForthState -> Either ForthError ForthState
doSwap (FS (x:y:xs) d) = Right (FS (y:x:xs) d)
doSwap _ = Left StackUnderflow

doOver :: ForthState -> Either ForthError ForthState
doOver (FS (x:y:xs) d) = Right (FS (y:x:y:xs) d)
doOver _ = Left StackUnderflow

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

-- Check if a token is purely digits
isNumber :: Text -> Bool
isNumber t = T.all C.isDigit t

-- Convert digit-text to Int
readNumber :: Text -> Int
readNumber = read . T.unpack
