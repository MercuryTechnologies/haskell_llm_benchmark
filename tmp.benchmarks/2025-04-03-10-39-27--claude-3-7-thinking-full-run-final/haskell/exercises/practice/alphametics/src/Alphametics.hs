module Alphametics (solve) where

import Data.Char (isAlpha, toUpper)
import Data.List (nub, permutations)
import Data.Maybe (listToMaybe)

-- Function to solve alphametics puzzles
solve :: String -> Maybe [(Char, Int)]
solve puzzle
  | length uniqueLetters > 10 = Nothing  -- More than 10 unique letters, no solution possible
  | otherwise = listToMaybe solutions
  where
    -- Clean the puzzle: keep only alphabets, +, and =
    cleanPuzzle = filter (\c -> isAlpha c || c `elem` "+=") $ map toUpper puzzle
    
    -- Find the position of the equals sign
    eqPos = length $ takeWhile (/= '=') cleanPuzzle
    
    -- Split the equation into left and right sides
    leftSide = take eqPos cleanPuzzle
    rightSide = drop (eqPos + 1) cleanPuzzle
    
    -- Split the left side into addends
    addends = filter (not . null) $ splitAddends leftSide
    
    -- Helper function to split addends
    splitAddends "" = []
    splitAddends str = 
        let (addend, rest) = span (/= '+') str
        in if null rest then [addend] else addend : splitAddends (drop 1 rest)
    
    -- Get unique letters from the puzzle
    uniqueLetters = nub $ filter isAlpha cleanPuzzle
    
    -- Generate all possible assignments
    possibleAssignments = map (zip uniqueLetters) $ permutations [0..9]
    
    -- Check if an assignment is valid
    isValidAssignment assignment =
        not (hasLeadingZero assignment rightSide || any (hasLeadingZero assignment) addends) && 
        sum (map (evaluateWord assignment) addends) == evaluateWord assignment rightSide
    
    -- Check if a word has a leading zero with the given assignment
    hasLeadingZero assignment word = lookupDigit (head word) == 0 && length word > 1
      where lookupDigit c = case lookup c assignment of Just d -> d; _ -> error "Unknown letter"
    
    -- Evaluate a word given an assignment of digits to letters
    evaluateWord assignment word = 
        foldl (\acc c -> acc * 10 + lookupDigit c) 0 word
      where lookupDigit c = case lookup c assignment of Just d -> d; _ -> error "Unknown letter"
    
    -- Solutions that satisfy all constraints
    solutions = filter isValidAssignment possibleAssignments
