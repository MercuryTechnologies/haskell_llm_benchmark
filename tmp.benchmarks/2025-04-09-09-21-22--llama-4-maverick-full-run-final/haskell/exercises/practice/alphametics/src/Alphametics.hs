module Alphametics (solve) where

import Data.Char (isAlpha, isUpper, toUpper, digitToInt)
import Data.List (nub, (\\))
import Data.Maybe (catMaybes, isJust)

solve :: String -> Maybe [(Char, Int)]
solve puzzle = 
  let 
    equation = parseEquation puzzle
    chars = nub $ concatMap (map toUpper) equation
    firstChars = map (head . map toUpper) equation
  in 
    findSolution chars firstChars equation

parseEquation :: String -> [String]
parseEquation puzzle = 
  let 
    wordList = words $ map toUpper puzzle
    (operands, result) = 
      case wordList of 
        (x:xs) -> (init xs, last xs)
        _ -> error "Invalid input"
  in 
    operands ++ [result]

findSolution :: String -> [Char] -> [String] -> Maybe [(Char, Int)]
findSolution chars firstChars equation = 
  head $ catMaybes $ map (tryAssignment chars firstChars equation) (permutations [0..9])
  where
    tryAssignment :: String -> [Char] -> [String] -> [Int] -> Maybe [(Char, Int)]
    tryAssignment chars firstChars equation permutation = 
      let 
        assignment = zip chars permutation
      in 
        if isValidAssignment assignment firstChars equation then Just assignment else Nothing

    isValidAssignment :: [(Char, Int)] -> [Char] -> [String] -> Bool
    isValidAssignment assignment firstChars equation = 
      let 
        numbers = map (read . map (show . fromJust . (`lookup` assignment))) equation
      in 
        all (\x -> fromJust (x `lookup` assignment) /= 0) firstChars && 
        sum (init numbers) == last numbers

    fromJust :: Maybe a -> a
    fromJust (Just x) = x
    fromJust Nothing = error "fromJust: Nothing"

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x:ys | x <- xs, ys <- permutations (xs \\ [x])]
