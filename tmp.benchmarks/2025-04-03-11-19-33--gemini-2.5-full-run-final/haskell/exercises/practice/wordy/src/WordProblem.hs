module WordProblem (answer) where

import Text.Read (readMaybe)
import Data.Maybe (isJust)

-- Main function to parse and evaluate the word problem.
answer :: String -> Maybe Integer
answer problem =
    case words (removeQuestionMark problem) of
        ("What":"is":rest) -> parseExpr rest -- Process the expression part
        _                  -> Nothing      -- Invalid start

-- Removes the trailing question mark if it exists.
removeQuestionMark :: String -> String
removeQuestionMark s
    | not (null s) && last s == '?' = init s
    | otherwise                     = s

-- Parses the initial number and starts the evaluation loop.
parseExpr :: [String] -> Maybe Integer
parseExpr [] = Nothing -- "What is?" is invalid.
parseExpr (numStr:rest) =
    case readMaybe numStr :: Maybe Integer of
        Nothing -> Nothing -- First part must be a number.
        Just initialVal -> evalLoop initialVal rest -- Start evaluation with the first number.

-- Recursively evaluates the expression from left to right.
evalLoop :: Integer -> [String] -> Maybe Integer
evalLoop currentVal [] = Just currentVal -- End of expression, return the result.
evalLoop currentVal (op:rest) =
    case op of
        "plus"       -> processSimpleOp (+) currentVal rest
        "minus"      -> processSimpleOp (-) currentVal rest
        "multiplied" -> processComplexOp (*) currentVal rest
        "divided"    -> processComplexOp div currentVal rest
        _            -> Nothing -- Unknown operation or invalid sequence.

-- Processes simple operations (plus, minus) which are followed directly by a number.
processSimpleOp :: (Integer -> Integer -> Integer) -> Integer -> [String] -> Maybe Integer
processSimpleOp op currentVal (numStr:rest) =
    applyOp op currentVal numStr >>= flip evalLoop rest -- Apply op and continue loop.
processSimpleOp _ _ _ = Nothing -- Operation must be followed by a number.

-- Processes complex operations (multiplied, divided) which expect "by" followed by a number.
processComplexOp :: (Integer -> Integer -> Integer) -> Integer -> [String] -> Maybe Integer
processComplexOp op currentVal ("by":numStr:rest) =
    applyOp op currentVal numStr >>= flip evalLoop rest -- Apply op and continue loop.
processComplexOp _ _ _ = Nothing -- Operation must be followed by "by" and a number.

-- Helper function to parse the next number string and apply the binary operation.
applyOp :: (Integer -> Integer -> Integer) -> Integer -> String -> Maybe Integer
applyOp op val1 numStr =
    case readMaybe numStr :: Maybe Integer of
        Nothing -> Nothing -- Failed to parse the number string.
        Just val2 -> Just (op val1 val2) -- Apply operation and return result.
