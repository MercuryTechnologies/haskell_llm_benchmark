module Alphametics (solve) where

import Data.List (nub, permutations)
import Data.Maybe (listToMaybe)
import Data.Char (isAlpha)

solve :: String -> Maybe [(Char, Int)]
solve puzzle = listToMaybe $ filter isValidSolution possibleSolutions
  where
    wordsInPuzzle = words $ map (\c -> if c == '+' || c == '=' then ' ' else c) puzzle
    uniqueLetters = nub $ filter isAlpha puzzle
    possibleSolutions = map (zip uniqueLetters) (permutations [0..9])
    
    isValidSolution solution = all (not . hasLeadingZero solution) wordsInPuzzle && isCorrectSum solution
      where
        hasLeadingZero solution word = let num = wordToNumber solution word
                                       in length word > 1 && lookupDigit (head word) solution == 0
        isCorrectSum solution = let numbers = map (wordToNumber solution) wordsInPuzzle
                                in sum (init numbers) == last numbers

    wordToNumber solution word = foldl (\acc c -> acc * 10 + lookupDigit c solution) 0 word
    lookupDigit c solution = maybe 0 id (lookup c solution)
