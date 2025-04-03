module WordProblem (answer) where

import Data.Char (isDigit)
import Data.List (isPrefixOf, isSuffixOf)

-- | Parse and evaluate a math word problem
answer :: String -> Maybe Integer
answer problem
  | not (isPrefixOf "What is" problem) || not ("?" `isSuffixOf` problem) = Nothing
  | otherwise = 
      let tokens = words $ init $ drop 7 problem  -- Remove "What is" and "?"
      in evaluate tokens

-- | Evaluate the mathematical expression
evaluate :: [String] -> Maybe Integer
evaluate [] = Nothing
evaluate (first:rest) = do
  n <- readNumber first
  evalOps n rest

-- | Evaluate operations
evalOps :: Integer -> [String] -> Maybe Integer
evalOps acc [] = Just acc
evalOps acc (op:rest)
  | op == "plus" && not (null rest) = do
      n <- readNumber (head rest)
      evalOps (acc + n) (tail rest)
  | op == "minus" && not (null rest) = do
      n <- readNumber (head rest)
      evalOps (acc - n) (tail rest)
  | op == "multiplied" && length rest >= 2 && head rest == "by" = do
      n <- readNumber (rest !! 1)
      evalOps (acc * n) (drop 2 rest)
  | op == "divided" && length rest >= 2 && head rest == "by" = do
      n <- readNumber (rest !! 1)
      if n == 0 then Nothing else evalOps (acc `div` n) (drop 2 rest)
  | otherwise = Nothing

-- | Read a number from a string
readNumber :: String -> Maybe Integer
readNumber str
  | all isDigit str = Just (read str)
  | head str == '-' && all isDigit (tail str) = Just (read str)
  | otherwise = Nothing
