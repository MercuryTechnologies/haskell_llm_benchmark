module Alphametics (solve) where

import Data.List (nub, permutations, find)
import Data.Maybe (isJust)

solve :: String -> Maybe [(Char, Int)]
solve puzzle = findSolution (words puzzle)

findSolution :: [String] -> Maybe [(Char, Int)]
findSolution wordsList = 
    let uniqueChars = nub (concat wordsList)
        numPermutations = permutations [0..9]
    in find (\digits -> isValidSolution wordsList uniqueChars digits) numPermutations

isValidSolution :: [String] -> String -> [Int] -> Bool
isValidSolution wordsList uniqueChars digits = 
    let charToDigit = zip uniqueChars digits
        wordValues = map (wordToValue charToDigit) wordsList
        (sumWords, resultWord) = (init wordValues, last wordValues)
    in sum sumWords == resultWord && all (not . isLeadingZero charToDigit) wordsList

wordToValue :: [(Char, Int)] -> String -> Int
wordToValue charToDigit word = sum [10 ^ i * digit | (c, digit) <- zip (reverse word) (map snd charToDigit), let i = length word - 1 - (length word - 1 - fromEnum c)]

isLeadingZero :: [(Char, Int)] -> String -> Bool
isLeadingZero charToDigit word = 
    case word of
        [] -> False
        (x:_) -> lookup x charToDigit == Just 0
