module Alphametics (solve) where

import Data.List (nub, permutations, sort)
import Data.Char (isAlpha, isSpace)

solve :: String -> Maybe [(Char, Int)]
solve puzzle =
  let cleanPuzzle = filter (not . isSpace) puzzle
      (lhs, rest) = break (=='=') cleanPuzzle
      -- We assume the input is well-formed, i.e. contains an '=' sign.
      (_eq, rhs) = splitAt 1 rest
      leftWords = splitOn '+' lhs
      rightWord = rhs
      allWords = leftWords ++ [rightWord]
      letters = sort (nub (filter isAlpha puzzle))
      leadLetters = nub [ head w | w <- allWords, length w > 1 ]
      digits = [0..9]
      assignments = [ zip letters perm
                    | perm <- permutations digits, length perm >= length letters
                    , isValidLeading leadLetters (zip letters perm)
                    ]
  in findValid leftWords rightWord assignments

isValidLeading :: [Char] -> [(Char, Int)] -> Bool
isValidLeading lead mapping = all (\ch -> lookup ch mapping /= Just 0) lead

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delim s =
  let (word, rest) = break (== delim) s
  in word : case rest of
              []      -> []
              (_:xs)  -> splitOn delim xs

wordToNumber :: [(Char, Int)] -> String -> Int
wordToNumber mapping word = foldl (\acc c -> acc * 10 + getDigit c mapping) 0 word
  where
    getDigit c mapping' = case lookup c mapping' of
                           Just d  -> d
                           Nothing -> 0

findValid :: [String] -> String -> [[(Char, Int)]] -> Maybe [(Char, Int)]
findValid _ _ [] = Nothing
findValid leftWords rightWord (m:ms) =
  let leftSum = sum (map (wordToNumber m) leftWords)
      rightVal = wordToNumber m rightWord
  in if leftSum == rightVal then Just m else findValid leftWords rightWord ms
