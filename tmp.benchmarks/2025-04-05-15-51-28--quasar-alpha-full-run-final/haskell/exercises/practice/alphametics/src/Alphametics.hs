module Alphametics (solve) where

import Data.Char (isAlpha, isUpper)
import Data.List (nub, permutations)
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as Map

solve :: String -> Maybe [(Char, Int)]
solve puzzle = listToMaybe solutions
  where
    (lhsWords, rhsWord) = parsePuzzle puzzle
    allWords = lhsWords ++ [rhsWord]
    uniqueLetters = nub $ concat allWords
    leadingLetters = nub $ map head allWords

    solutions =
      [ zip uniqueLetters digits
      | digits <- permutations [0..9]
      , let mapping = Map.fromList (zip uniqueLetters digits)
      , all (\c -> Map.findWithDefault 0 c mapping /= 0) leadingLetters
      , let lhsSum = sum (map (wordToNumber mapping) lhsWords)
      , let rhsNum = wordToNumber mapping rhsWord
      , lhsSum == rhsNum
      ]

parsePuzzle :: String -> ([String], String)
parsePuzzle s =
  let (lhs, rhs) = break (== '=') s
      lhsWords = words $ map replacePlus lhs
      rhsWord = filter isAlpha rhs
  in (lhsWords, rhsWord)
  where
    replacePlus c
      | c == '+' = ' '
      | isAlpha c = c
      | otherwise = ' '

wordToNumber :: Map.Map Char Int -> String -> Int
wordToNumber mapping = foldl (\acc c -> acc * 10 + Map.findWithDefault 0 c mapping) 0
