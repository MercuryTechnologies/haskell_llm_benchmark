module WordProblem (answer) where

import Data.Char (isDigit, isSpace)
import Data.List (stripPrefix)
import Text.Read (readMaybe)

answer :: String -> Maybe Integer
answer problem = parseQuestion (trim problem)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

parseQuestion :: String -> Maybe Integer
parseQuestion s = do
    s1 <- stripPrefix "What is " s
    s2 <- stripSuffix "?" s1
    parseExpression s2

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix str = reverse <$> stripPrefix (reverse suffix) (reverse str)

parseExpression :: String -> Maybe Integer
parseExpression s = do
    (n, rest) <- parseNumber s
    evalSequence n rest

parseNumber :: String -> Maybe (Integer, String)
parseNumber s = do
    let (numStr, rest) = span (\c -> isDigit c || c == '-') (dropWhile isSpace s)
    n <- readMaybe numStr
    return (n, rest)

evalSequence :: Integer -> String -> Maybe Integer
evalSequence acc s =
    case dropWhile isSpace s of
        "" -> Just acc
        rest -> do
            (op, s1) <- parseOperator rest
            (n, s2) <- parseNumber s1
            let acc' = applyOp op acc n
            evalSequence acc' s2

data Operator = Plus | Minus | Multiplied | Divided deriving (Eq, Show)

parseOperator :: String -> Maybe (Operator, String)
parseOperator s =
    asum
        [ parseOp "plus" Plus s
        , parseOp "minus" Minus s
        , parseOp "multiplied by" Multiplied s
        , parseOp "divided by" Divided s
        ]

parseOp :: String -> Operator -> String -> Maybe (Operator, String)
parseOp kw op s = do
    rest <- stripPrefix kw (dropWhile isSpace s)
    return (op, rest)

applyOp :: Operator -> Integer -> Integer -> Integer
applyOp Plus       = (+)
applyOp Minus      = (-)
applyOp Multiplied = (*)
applyOp Divided    = div

asum :: [Maybe a] -> Maybe a
asum [] = Nothing
asum (x:xs) = case x of
    Just _  -> x
    Nothing -> asum xs
