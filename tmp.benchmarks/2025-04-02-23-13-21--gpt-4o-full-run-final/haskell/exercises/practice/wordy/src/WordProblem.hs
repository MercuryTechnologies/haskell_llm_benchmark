module WordProblem (answer) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

data Operation = Add | Subtract | Multiply | Divide deriving (Show)
data Expression = Number Integer | Operation Operation Expression Expression deriving (Show)

parseNumber :: Parser Expression
parseNumber = do
    num <- many1 (digit <|> char '-')
    return $ Number (read num)

parseOperation :: Parser Operation
parseOperation = (string "plus" >> return Add)
             <|> (string "minus" >> return Subtract)
             <|> (string "multiplied by" >> return Multiply)
             <|> (string "divided by" >> return Divide)

parseExpression :: Parser Expression
parseExpression = do
    spaces
    num1 <- parseNumber
    spaces
    ops <- many (try parseOpExpr)
    return $ foldl (\acc (op, num) -> Operation op acc num) num1 ops
  where
    parseOpExpr = do
        op <- parseOperation
        spaces
        num <- parseNumber
        spaces
        return (op, num)

evaluate :: Expression -> Integer
evaluate (Number n) = n
evaluate (Operation Add e1 e2) = evaluate e1 + evaluate e2
evaluate (Operation Subtract e1 e2) = evaluate e1 - evaluate e2
evaluate (Operation Multiply e1 e2) = evaluate e1 * evaluate e2
evaluate (Operation Divide e1 e2) = evaluate e1 `div` evaluate e2

answer :: String -> Maybe Integer
answer problem = case parse parseExpression "" (stripPrefix "What is " problem) of
    Left _ -> Nothing
    Right expr -> Just (evaluate expr)

stripPrefix :: String -> String -> String
stripPrefix prefix str = fromMaybe str (stripPrefix' prefix str)
  where
    stripPrefix' [] ys = Just ys
    stripPrefix' _ [] = Nothing
    stripPrefix' (x:xs) (y:ys)
      | x == y    = stripPrefix' xs ys
      | otherwise = Nothing
