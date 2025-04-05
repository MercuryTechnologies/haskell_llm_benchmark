module WordProblem (answer) where

import Data.Attoparsec.Text
import Data.Text (Text, pack)
import Control.Applicative ((<|>))
import Data.Maybe (listToMaybe)

answer :: String -> Maybe Integer
answer problem = case parseOnly parseProblem (pack problem) of
    Right (Just x) -> Just x
    _ -> Nothing

parseProblem :: Parser (Maybe Integer)
parseProblem = do
    _ <- string "What is "
    num <- decimal
    rest <- many' parseOperation
    _ <- char '?'
    return $ Just (foldl (flip ($)) num rest)

parseOperation :: Parser (Integer -> Integer)
parseOperation = do
    skipSpace
    op <- parsePlus <|> parseMinus <|> parseMultiply <|> parseDivide
    skipSpace
    num <- decimal
    return (`op` num)

parsePlus :: Parser (Integer -> Integer -> Integer)
parsePlus = string "plus" >> return (+)

parseMinus :: Parser (Integer -> Integer -> Integer)
parseMinus = string "minus" >> return (-)

parseMultiply :: Parser (Integer -> Integer -> Integer)
parseMultiply = string "multiplied by" >> return (*)

parseDivide :: Parser (Integer -> Integer -> Integer)
parseDivide = string "divided by" >> return div
