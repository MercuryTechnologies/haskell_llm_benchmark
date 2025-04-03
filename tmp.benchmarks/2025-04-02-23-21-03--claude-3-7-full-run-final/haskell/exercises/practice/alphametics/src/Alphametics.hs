module Alphametics (solve) where

import Data.Char (isAlpha, isUpper)
import Data.List (nub, (\\))
import Data.Maybe (listToMaybe)

-- | Solve an alphametics puzzle
solve :: String -> Maybe [(Char, Int)]
solve puzzle = listToMaybe $ solveEquation equation
  where
    equation = parseEquation puzzle

-- | Parse the equation from the puzzle string
parseEquation :: String -> ([String], String)
parseEquation puzzle = (addends, result)
  where
    parts = words $ filter (\c -> isAlpha c || c == '+' || c == '=') puzzle
    (addendParts, resultParts) = break (== "=") parts
    addends = filter (/= "+") addendParts
    result = head $ tail resultParts  -- Take the part after "="

-- | Solve the equation by trying different digit assignments
solveEquation :: ([String], String) -> [[(Char, Int)]]
solveEquation (addends, result) = 
    filter (isValidSolution addends result) $ generateAssignments letters addends result
  where
    letters = nub $ filter isUpper $ concat (addends ++ [result])

-- | Generate all possible digit assignments for the letters
generateAssignments :: [Char] -> [String] -> String -> [[(Char, Int)]]
generateAssignments letters addends result = assignDigits letters [0..9] []
  where
    assignDigits [] _ acc = [acc]
    assignDigits (l:ls) availDigits acc = 
        concatMap (\d -> assignDigits ls (availDigits \\ [d]) ((l, d):acc)) validDigits
      where
        validDigits = if isLeadingChar l
                      then filter (/= 0) availDigits
                      else availDigits
        
        isLeadingChar c = c `elem` leadingChars
        leadingChars = map head (addends ++ [result])

-- | Check if a digit assignment produces a valid solution
isValidSolution :: [String] -> String -> [(Char, Int)] -> Bool
isValidSolution addends result assignment = 
    sum (map (wordToNum assignment) addends) == wordToNum assignment result

-- | Convert a word to a number using the digit assignment
wordToNum :: [(Char, Int)] -> String -> Integer
wordToNum assignment word = 
    foldl (\acc c -> acc * 10 + toInteger (lookupDigit c)) 0 word
  where
    lookupDigit c = case lookup c assignment of
                      Just d -> d
                      Nothing -> error $ "No digit assigned to " ++ [c]
