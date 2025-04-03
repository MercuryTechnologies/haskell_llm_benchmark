module WordProblem (answer) where

import Data.Char (isDigit)
import Data.List (isPrefixOf)

-- | Parse and evaluate a math word problem
answer :: String -> Maybe Integer
answer problem
  | not (isPrefixOf "What is" problem) || not ("?" `isPrefixOf` reverse (dropWhile (==' ') problem)) = Nothing
  | otherwise = do
      let questionText = drop 8 problem  -- Remove "What is"
          tokens = words $ if null questionText then "" else init questionText  -- Remove "?" safely
      case tokens of
        [] -> Nothing  -- Reject problems with no operands or operators
        _  -> do
          (result, remaining) <- parseNumber tokens
          parseOperations result remaining

-- | Parse a number from the beginning of the token list
parseNumber :: [String] -> Maybe (Integer, [String])
parseNumber [] = Nothing
parseNumber (x:xs)
  | all (\c -> isDigit c || c == '-') x = case reads x of
      [(n, "")] -> Just (n, xs)
      _         -> Nothing
  | otherwise = Nothing

-- | Parse operations and apply them in sequence
parseOperations :: Integer -> [String] -> Maybe Integer
parseOperations result [] = Just result
parseOperations result tokens = do
  (op, rest) <- parseOperation tokens
  case rest of
    [] -> Nothing
    _  -> do
      (num, remaining) <- parseNumber rest
      let newResult = applyOperation result op num
      parseOperations newResult remaining

-- | Parse an operation from the token list
parseOperation :: [String] -> Maybe (String, [String])
parseOperation [] = Nothing
parseOperation ("plus":xs) = Just ("plus", xs)
parseOperation ("minus":xs) = Just ("minus", xs)
parseOperation ("multiplied":"by":xs) = Just ("multiplied by", xs)
parseOperation ("divided":"by":xs) = Just ("divided by", xs)
parseOperation _ = Nothing

-- | Apply an operation to two numbers
applyOperation :: Integer -> String -> Integer -> Integer
applyOperation x "plus" y = x + y
applyOperation x "minus" y = x - y
applyOperation x "multiplied by" y = x * y
applyOperation x "divided by" y = x `div` y
applyOperation _ _ _ = error "Unsupported operation"
