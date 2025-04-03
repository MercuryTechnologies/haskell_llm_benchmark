module WordProblem (answer) where

import Text.Read (readMaybe)
import Data.List (isInfixOf)

answer :: String -> Maybe Integer
answer problem
    | "What is" `isInfixOf` problem = evaluate (words (drop 8 problem))
    | otherwise = Nothing

evaluate :: [String] -> Maybe Integer
evaluate [] = Nothing
evaluate [x] = readMaybe x
evaluate (x:op:y:rest)
    | op == "plus" = case (readMaybe x, readMaybe y) of
        (Just a, Just b) -> evaluate (show (a + b) : rest)
        _ -> Nothing
    | op == "minus" = case (readMaybe x, readMaybe y) of
        (Just a, Just b) -> evaluate (show (a - b) : rest)
        _ -> Nothing
    | op == "multiplied" = case (readMaybe x, readMaybe y) of
        (Just a, Just b) -> evaluate (show (a * b) : rest)
        _ -> Nothing
    | op == "divided" = case (readMaybe x, readMaybe y) of
        (Just a, Just b) -> if b /= 0 then evaluate (show (a `div` b) : rest) else Nothing
        _ -> Nothing
    | otherwise = Nothing
