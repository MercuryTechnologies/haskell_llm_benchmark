module WordProblem (answer) where

import Text.Read (readMaybe)

answer :: String -> Maybe Integer
answer problem =
    case stripQuestion problem of
      Nothing -> Nothing
      Just expr -> do
        tokens <- tokenize expr
        evaluateTokens tokens

-- Remove "What is " prefix and trailing "?" if present
stripQuestion :: String -> Maybe String
stripQuestion str =
    let prefix = "What is "
        suffix = "?"
     in if prefix `isPrefixOf` str && suffix `isSuffixOf` str
           then Just (drop (length prefix) (take (length str - length suffix) str))
           else Nothing

-- Check if str starts with prefix
isPrefixOf :: String -> String -> Bool
isPrefixOf [] _          = True
isPrefixOf _ []          = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Check if str ends with suffix
isSuffixOf :: String -> String -> Bool
isSuffixOf suf str =
    let lenSuf = length suf
        lenStr = length str
     in lenSuf <= lenStr && drop (lenStr - lenSuf) str == suf

-- Break the expression into either numbers or recognized operations.
-- We handle "plus", "minus", "multiplied by", "divided by" as operations.
tokenize :: String -> Maybe [Token]
tokenize = parseWords . words

data Token
    = TNumber Integer
    | TOpPlus
    | TOpMinus
    | TOpMultiply
    | TOpDivide
    deriving (Show)

parseWords :: [String] -> Maybe [Token]
parseWords [] = Nothing
parseWords ws = parseNext ws []

parseNext :: [String] -> [Token] -> Maybe [Token]
parseNext [] acc = Just acc
parseNext (w:rest) acc =
  case w of
    "plus"        -> parseNext rest (acc ++ [TOpPlus])
    "minus"       -> parseNext rest (acc ++ [TOpMinus])
    "multiplied"  ->
      case rest of
        ("by":r2) -> parseNext r2 (acc ++ [TOpMultiply])
        _         -> Nothing  -- syntax error
    "divided"     ->
      case rest of
        ("by":r2) -> parseNext r2 (acc ++ [TOpDivide])
        _         -> Nothing  -- syntax error
    _             -> do
      n <- readMaybe w
      parseNext rest (acc ++ [TNumber n])

-- Evaluate tokens from left to right ignoring standard precedence.
evaluateTokens :: [Token] -> Maybe Integer
evaluateTokens tokens = do
  (firstVal, rest) <- extractFirstNumber tokens
  pairs <- pairUp rest
  foldl applyOp (Just firstVal) pairs

extractFirstNumber :: [Token] -> Maybe (Integer, [Token])
extractFirstNumber (TNumber n : xs) = Just (n, xs)
extractFirstNumber _ = Nothing

pairUp :: [Token] -> Maybe [(Token, Integer)]
pairUp [] = Just []
pairUp (op : TNumber n : xs) = do
  tailPairs <- pairUp xs
  return ((op, n) : tailPairs)
pairUp _ = Nothing

applyOp :: Maybe Integer -> (Token, Integer) -> Maybe Integer
applyOp (Just currentVal) (TOpPlus, n)     = Just (currentVal + n)
applyOp (Just currentVal) (TOpMinus, n)    = Just (currentVal - n)
applyOp (Just currentVal) (TOpMultiply, n) = Just (currentVal * n)
applyOp (Just currentVal) (TOpDivide, n)   =
  if n == 0 then Nothing else Just (currentVal `div` n)
applyOp _ _ = Nothing
