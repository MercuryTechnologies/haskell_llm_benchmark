module WordProblem (answer) where

import Text.Read (readMaybe)

data Operation = Plus | Minus | Multiplied | Divided
  deriving (Eq, Show)

parseOperation :: String -> Maybe Operation
parseOperation "plus" = Just Plus
parseOperation "minus" = Just Minus
parseOperation "multiplied" = Just Multiplied
parseOperation "divided" = Just Divided
parseOperation _ = Nothing

parseProblem :: String -> Maybe [(Integer, Maybe Operation)]
parseProblem problem = do
  let problem' = drop 8 $ init problem -- remove "What is " and "?"
  let wordList = words problem'
  parseWords wordList

parseWords :: [String] -> Maybe [(Integer, Maybe Operation)]
parseWords [] = Just []
parseWords (w:ws) = do
  num <- readMaybe w :: Maybe Integer
  rest <- parseRest ws
  return ((num, Nothing) : rest)
  where
    parseRest :: [String] -> Maybe [(Integer, Maybe Operation)]
    parseRest [] = Just []
    parseRest (op1:op2:ws') = do
      op <- case (op1, op2) of
        ("multiplied", "by") -> Just Multiplied
        ("divided", "by") -> Just Divided
        _ -> Nothing
      rest <- parseWords ws'
      return ((0, Just op) : rest)
    parseRest (op:ws') = do
      op' <- parseOperation op
      rest <- parseWords ws'
      return ((0, Just op') : rest)
    parseRest _ = Nothing

evaluate :: [(Integer, Maybe Operation)] -> Maybe Integer
evaluate [(num, Nothing)] = Just num
evaluate ((num1, Nothing):(num2, Just op):rest) = do
  result <- case op of
    Plus -> Just (num1 + num2)
    Minus -> Just (num1 - num2)
    Multiplied -> Just (num1 * num2)
    Divided -> if num2 == 0 then Nothing else Just (num1 `div` num2)
  evaluate ((result, Nothing) : rest)
evaluate _ = Nothing

answer :: String -> Maybe Integer
answer problem = do
  expr <- parseProblem problem
  evaluate expr
