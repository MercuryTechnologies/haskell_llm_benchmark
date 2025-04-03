module WordProblem (answer) where

import Data.List (isPrefixOf, isSuffixOf)
import Text.Read (readMaybe)

answer :: String -> Maybe Integer
answer problem =
    -- Check proper prefix and suffix
    if not (isPrefixOf "What is " problem && isSuffixOf "?" problem)
      then Nothing
      else
        let -- Remove the prefix "What is " and the trailing "?"
            core = drop (length "What is ") (take (length problem - 1) problem)
        in parseAndCompute core

-- Parse the string (after removing "What is " and "?") and compute result
parseAndCompute :: String -> Maybe Integer
parseAndCompute s =
    let tokens = tokenize s
    in computeTokens tokens

-- Tokenize the question, combining "multiplied by" and "divided by" into single tokens
tokenize :: String -> [String]
tokenize = combine . words
  where
    combine ("multiplied":"by":xs) = "multiplied_by" : combine xs
    combine ("divided":"by":xs)    = "divided_by" : combine xs
    combine (x:xs)                 = x : combine xs
    combine []                     = []

-- Compute the result from a list of tokens: [num, op, num, op, ...], left to right
computeTokens :: [String] -> Maybe Integer
computeTokens [] = Nothing
computeTokens (first:rest) = do
    initialValue <- parseNumber first
    foldLeft initialValue rest

-- Folds the list of tokens in pairs: operation, then number, ignoring normal precedence
foldLeft :: Integer -> [String] -> Maybe Integer
foldLeft current [] = Just current
foldLeft _ [op] = Nothing  -- There's an op with no following number, invalid
foldLeft current (opStr:numStr:xs) = do
    opFunc <- parseOp opStr
    val <- parseNumber numStr
    -- Handle division by zero
    if opStr == "divided_by" && val == 0
      then Nothing
      else foldLeft (opFunc current val) xs

-- Parse a string as an Integer
parseNumber :: String -> Maybe Integer
parseNumber = readMaybe

-- Map an operation token to the corresponding function
parseOp :: String -> Maybe (Integer -> Integer -> Integer)
parseOp "plus"          = Just (+)
parseOp "minus"         = Just (-)
parseOp "multiplied_by" = Just (*)
parseOp "divided_by"    = Just div
parseOp _               = Nothing
