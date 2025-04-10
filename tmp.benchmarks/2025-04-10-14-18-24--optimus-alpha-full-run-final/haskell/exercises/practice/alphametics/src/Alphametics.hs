module Alphametics (solve) where

import Data.Char (isAlpha, isUpper)
import Data.List (nub, permutations, find, transpose)
import Data.Maybe (fromJust, isJust, mapMaybe)
import qualified Data.Map.Strict as Map

-- Parse the puzzle into (summands, result)
parsePuzzle :: String -> ([String], String)
parsePuzzle s =
    let (lhs, rhs) = break (== '=') s
        lhsWords = words $ map (\c -> if c == '+' then ' ' else c) lhs
        rhsWord = filter isAlpha rhs
    in (lhsWords, rhsWord)

-- Get all unique letters in the puzzle, in order of appearance
uniqueLetters :: [String] -> String -> [Char]
uniqueLetters summands result = nub $ concat (summands ++ [result])

-- Get all leading letters (cannot be zero)
leadingLetters :: [String] -> String -> [Char]
leadingLetters summands result = nub $ map head (filter (not . null) (summands ++ [result]))

-- Convert a word to a number using a mapping
wordToNumber :: Map.Map Char Int -> String -> Int
wordToNumber m = foldl (\acc c -> acc * 10 + fromJust (Map.lookup c m)) 0

-- Check if a mapping is valid (no leading zeros)
isValidMapping :: Map.Map Char Int -> [String] -> String -> Bool
isValidMapping m summands result =
    all (\w -> Map.findWithDefault 0 (head w) m /= 0) (filter (\w -> length w > 1) (summands ++ [result]))

-- Try all possible assignments
solve :: String -> Maybe [(Char, Int)]
solve puzzle =
    let (summands, result) = parsePuzzle puzzle
        letters = uniqueLetters summands result
        n = length letters
        leads = leadingLetters summands result
        digits = [0..9]
        possibleAssignments = filter (\p -> all (\(c, d) -> d /= 0 || not (c `elem` leads)) (zip letters p))
                                (permutations digits >>= (\p -> if n <= 10 then return (take n p) else []))
        tryAssignment ds =
            let m = Map.fromList (zip letters ds)
                sumWords = map (wordToNumber m) summands
                resultWord = wordToNumber m result
            in if isValidMapping m summands result && sum sumWords == resultWord
                then Just (Map.toList m)
                else Nothing
    in find isJust (map tryAssignment possibleAssignments) >>= id
