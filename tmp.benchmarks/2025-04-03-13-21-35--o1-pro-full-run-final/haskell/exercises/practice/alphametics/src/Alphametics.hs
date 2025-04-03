module Alphametics (solve) where

import Data.List (nub, permutations)
import Data.Char (isLetter)
import Data.Maybe (listToMaybe)

-- | Solve an alphametics puzzle (e.g. "SEND + MORE = MONEY").
-- | Each letter stands for a unique digit 0-9, and the sum must be correct.
-- | Returns the first valid letter→digit mapping found, or Nothing if none exist.
solve :: String -> Maybe [(Char, Int)]
solve puzzle =
    let puzzleNoSpaces = filter (/= ' ') puzzle
        (leftSide, rightSideWithEq) = break (== '=') puzzleNoSpaces
        rightSide = drop 1 rightSideWithEq  -- remove the '='
        lhsWords = splitPlus leftSide
        filteredLHS = filter (not . null) lhsWords
        allWords = filteredLHS ++ [rightSide]
        letters = nub (filter isLetter puzzleNoSpaces)
        -- Leading letters (cannot map to zero if word length > 1)
        leadingLetters = [head w | w <- allWords, length w > 1]
    in
      -- If there are more than 10 unique letters, it's unsolvable with digits 0-9
      if length letters > 10
      then Nothing
      else
          listToMaybe
            [ zip letters digits
            | digits <- permutations [0..9]
            , isValidSolution letters digits filteredLHS rightSide leadingLetters
            ]

-- | Split a string by '+' (avoids partial-pattern warnings).
splitPlus :: String -> [String]
splitPlus [] = [""]
splitPlus (c:cs)
  | c == '+'  = "" : splitPlus cs
  | otherwise =
      case splitPlus cs of
        (x:xs) -> (c:x) : xs
        []     -> [[c]]

-- | Check if a particular assignment of letters to digits satisfies all conditions:
-- | 1) Leading letters aren't zero (unless word length = 1).
-- | 2) Each word's numeric form has the same number of digits as the word (no extra carry).
-- | 3) The sum of the left side words equals the right side word.
isValidSolution :: [Char]         -- ^ Unique letters in puzzle
                -> [Int]          -- ^ Candidate digits for each letter
                -> [String]       -- ^ Left side words
                -> String         -- ^ Right side word
                -> [Char]         -- ^ Leading letters that can't map to zero
                -> Bool
isValidSolution letters digits lhs rhs leadingLetters =
    let dict = zip letters digits
        -- Leading letter cannot map to 0 when word length > 1
        leadingCheck = all (\c -> lookup c dict /= Just 0) leadingLetters

        -- Ensure that each word's numeric length matches its letter count
        lengthCheck = all (\w -> length (show (wordToNumber dict w)) == length w) (lhs ++ [rhs])

        sumLHS = sum (map (wordToNumber dict) lhs)
        sumRHS = wordToNumber dict rhs
    in leadingCheck && lengthCheck && (sumLHS == sumRHS)

-- | Convert a word to its number value given a mapping from letters to digits.
wordToNumber :: [(Char, Int)] -> String -> Int
wordToNumber dict = foldl (\acc c -> acc * 10 + lookupDigit c dict) 0

-- | Look up the digit for a single character in our mapping.
lookupDigit :: Char -> [(Char, Int)] -> Int
lookupDigit c dict =
  case lookup c dict of
    Just n  -> n
    Nothing -> 0  -- Should never happen if c is guaranteed to be in letters
