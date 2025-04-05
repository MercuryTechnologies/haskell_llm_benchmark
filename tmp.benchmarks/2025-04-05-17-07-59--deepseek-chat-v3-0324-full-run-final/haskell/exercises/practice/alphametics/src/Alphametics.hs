module Alphametics (solve) where

import Data.Char (isUpper)
import Data.List (nub, (\\))
import Data.Maybe (listToMaybe)
import Data.Map (Map, empty, insert, member, notMember, findWithDefault)
import Data.Foldable (foldl')

solve :: String -> Maybe [(Char, Int)]
solve puzzle = listToMaybe $ solvePuzzle letters firstLetters equation
  where
    -- Extract all unique letters in the puzzle
    letters = nub $ filter isUpper puzzle
    
    -- Split into left words and right word
    (leftWords, rightWord) = case words puzzle of
        ws -> (init (init ws), last ws)
    
    -- Letters that can't be zero (first letters of each word)
    firstLetters = nub $ map head (rightWord:leftWords)
    
    -- Create equation structure for evaluation
    equation = (leftWords, rightWord)

-- Backtracking solver with pruning
solvePuzzle :: [Char] -> [Char] -> ([String], String) -> [[(Char, Int)]]
solvePuzzle letters firstLetters equation = backtrack empty letters (reverse letters)
  where
    (leftWords, rightWord) = equation
    
    backtrack :: Map Char Int -> [Char] -> [Char] -> [[(Char, Int)]]
    backtrack _ [] _ = if isValidAssignment then [assignment] else []
      where
        assignment = foldl' (\acc (k,v) -> (k,v):acc) [] (empty :: Map Char Int)
        isValidAssignment = eval equation assignment
    backtrack assigned (c:cs) remainingDigits =
        [ solution
        | d <- remainingDigits
        , not (d == 0 && c `elem` firstLetters)  -- No leading zeros
        , let newAssigned = insert c d assigned
        , not (hasContradiction newAssigned)
        , solution <- backtrack newAssigned cs (remainingDigits \\ [d])
        ]
    
    -- Check for contradictions in partial assignments
    hasContradiction assigned =
        let evalColumn col = sum (map (getDigit assigned) (map (!! col) leftWords)) `mod` 10
            expectedDigit col = getDigit assigned (rightWord !! col)
            maxCol = length rightWord - 1
        in any (\col -> evalColumn col /= expectedDigit col) [0..maxCol]
    
    -- Safe digit lookup with default for unassigned letters
    getDigit assigned c = findWithDefault (-1) c assigned

-- Evaluate if the assignment satisfies the equation
eval :: ([String], String) -> Map Char Int -> Bool
eval (leftWords, rightWord) assignment = 
    let wordToNum word = foldl' (\acc c -> acc * 10 + findWithDefault 0 c assignment) 0 word
        leftSum = sum $ map wordToNum leftWords
        rightNum = wordToNum rightWord
    in leftSum == rightNum
