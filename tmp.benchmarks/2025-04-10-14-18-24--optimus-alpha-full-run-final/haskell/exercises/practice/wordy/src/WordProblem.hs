module WordProblem (answer) where

import Data.Char (isDigit, isSpace)
import Data.List (stripPrefix)
import Text.Read (readMaybe)

data Op = Plus | Minus | Times | Div deriving (Eq, Show)

-- Parse a single operation word into Op
parseOp :: String -> Maybe Op
parseOp "plus"        = Just Plus
parseOp "minus"       = Just Minus
parseOp "multiplied"  = Just Times
parseOp "divided"     = Just Div
parseOp _             = Nothing

-- Remove "What is " prefix and "?" suffix, if present
stripQuestion :: String -> Maybe String
stripQuestion s = do
    s1 <- stripPrefix "What is " s
    let s2 = if not (null s1) && last s1 == '?' then init s1 else s1
    return s2

-- Split the string into tokens, handling "multiplied by" and "divided by"
tokenize :: String -> [String]
tokenize = go . words
  where
    go [] = []
    go ("multiplied":"by":rest) = "multiplied" : go rest
    go ("divided":"by":rest)    = "divided"    : go rest
    go (x:xs)                   = x : go xs

-- Parse the sequence of numbers and operations
parseSequence :: [String] -> Maybe [Either Integer Op]
parseSequence [] = Nothing
parseSequence xs = sequence $ map parseToken' xs
  where
    parseToken' s =
      case parseOp s of
        Just op -> Just (Right op)
        Nothing -> case readMaybe s :: Maybe Integer of
            Just n  -> Just (Left n)
            Nothing -> Nothing

-- Evaluate the sequence left-to-right
evalSequence :: [Either Integer Op] -> Maybe Integer
evalSequence (Left n : rest) = go n rest
  where
    go acc [] = Just acc
    go acc (Right op : Left n2 : xs) =
        let acc' = case op of
                        Plus  -> Just (acc + n2)
                        Minus -> Just (acc - n2)
                        Times -> Just (acc * n2)
                        Div   -> if n2 == 0 then Nothing else Just (acc `div` n2)
        in acc' >>= \v -> go v xs
    go _ _ = Nothing
evalSequence _ = Nothing

answer :: String -> Maybe Integer
answer s = do
    stripped <- stripQuestion s
    let tokens = tokenize stripped
    seq <- parseSequence tokens
    evalSequence seq
