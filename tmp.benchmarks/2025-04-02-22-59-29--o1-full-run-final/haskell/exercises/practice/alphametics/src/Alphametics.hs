module Alphametics (solve) where

import Data.Char (isSpace)
import Data.List (nub, permutations)

-- | Solve an alphametics puzzle, returning a valid mapping of letters to digits
--   if one exists, or Nothing if no valid solution is found.
--   Example puzzle: "SEND + MORE = MONEY"
solve :: String -> Maybe [(Char, Int)]
solve puzzle =
  let (leftSide, rightSide) = splitAtEquals puzzle
      leftWords = splitWords leftSide
      rightWord = trim rightSide
      letters   = nub (concat (leftWords ++ [rightWord]))
  in
    -- If there are more than 10 unique letters, no solution is possible
    if length letters > 10
      then Nothing
      else findSolution leftWords rightWord letters

--------------------------------------------------------------------------------
-- Puzzling logic
--------------------------------------------------------------------------------

-- Try all permutations of digits for the given letters.
-- Return the first mapping that satisfies the puzzle,
-- or Nothing if none do.
findSolution :: [String] -> String -> [Char] -> Maybe [(Char, Int)]
findSolution leftWords rightWord letters =
  let allDigits = [0..9]
      possibleDigitSets = permutations allDigits
      neededDigitsCount = length letters
  in go possibleDigitSets neededDigitsCount
  where
    go [] _ = Nothing
    go (digitsCandidate:rest) neededCount
      | length digitsCandidate < neededCount = Nothing
      | otherwise =
          let candidate = zip letters (take neededCount digitsCandidate)
          in if isValidMapping leftWords rightWord candidate
               then Just candidate
               else go rest neededCount

-- Check if the candidate mapping is valid:
--   1. No leading word letter is mapped to zero (if the word has multiple letters).
--   2. Verify column-wise addition, ignoring any final carry if there's no extra letter for it.
isValidMapping :: [String] -> String -> [(Char, Int)] -> Bool
isValidMapping leftWords rightWord mapping =
  not (any (leadingZero mapping) leftWords)
  && not (leadingZero mapping rightWord)
  && columnAdditionCheck leftWords rightWord mapping

-- Perform a column-wise addition check:
--   sum(all left word digits for column + carry) mod 10 = digit of right word for that column
--   carry is carried over to the next column.
--   If at the end we still have carry but no more digits for the right word, we ignore it.
columnAdditionCheck :: [String] -> String -> [(Char, Int)] -> Bool
columnAdditionCheck leftWords rightWord mapping =
  let maxLen = maximum (map length (leftWords ++ [rightWord]))
      revLeftWords = map reverse leftWords
      revRightWord = reverse rightWord
  in go 0 0
  where
    go col carry
      | col >= maxLen = True
      | otherwise =
          let sumLeft = sum [ digitAt w col mapping | w <- revLeftWords ]
              rightDigit = digitAt revRightWord col mapping
              total = sumLeft + carry
              resultDigit = total `mod` 10
              newCarry = total `div` 10
          in if resultDigit == rightDigit
               then go (col + 1) newCarry
               else False

-- Get the digit for a word at a given (reversed) column index.
digitAt :: String -> Int -> [(Char, Int)] -> Int
digitAt w col mapping =
  if col < length w
     then digitFor (w !! col) mapping
     else 0

-- Given a mapping, convert a word to its numeric value. (Unused in final check, but left intact)
wordValue :: [(Char, Int)] -> String -> Int
wordValue mapping = foldl (\acc c -> acc * 10 + digitFor c mapping) 0

-- Get the digit for a particular letter from our mapping.
digitFor :: Char -> [(Char, Int)] -> Int
digitFor c m =
  case lookup c m of
    Just d  -> d
    Nothing -> error "Letter not found in mapping (should not happen)."

-- Check if a word starts with a zero in the given mapping,
-- only relevant if the word has multiple letters.
leadingZero :: [(Char, Int)] -> String -> Bool
leadingZero _ [] = False
leadingZero _ [_] = False
leadingZero m (x:_) =
  case lookup x m of
    Just 0 -> True
    _      -> False

--------------------------------------------------------------------------------
-- Parser helpers
--------------------------------------------------------------------------------

-- Split the puzzle into (leftPart, rightPart) around the '='
splitAtEquals :: String -> (String, String)
splitAtEquals s =
  let (left, rest) = break (== '=') s
  in (trim left, trim (drop 1 rest))

-- Split a string of words around '+' signs
splitWords :: String -> [String]
splitWords s =
  let pieces = splitOn '+' s
  in map trim pieces

-- A generic splitOn function.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn delim xs =
  let (before, remainder) = break (== delim) xs
  in before : case remainder of
                []      -> []
                (_:rs)  -> splitOn delim rs

-- Trim whitespace from both ends
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (Char -> Bool) -> String -> String
dropWhileEnd p = reverse . dropWhile p . reverse
