module Alphametics (solve) where

import Data.List (nub, permutations, find)
import Data.Char (isLetter)
import Data.Maybe (mapMaybe)

-- | Represents the parsed puzzle with addends and the result word.
data Puzzle = Puzzle { addends :: [String], result :: String } deriving Show

-- | Parses the input string into a Puzzle data structure.
-- Returns Nothing if the format is invalid.
parsePuzzle :: String -> Maybe Puzzle
parsePuzzle puzzleStr =
    case break (== '=') puzzleStr of
        (left, '=':'=':right) ->
            let ws = words $ filter (\c -> isLetter c || c == '+') left
                adds = filter (/= "+") ws
                res = filter isLetter right
            in if not (null adds) && not (null res) && all (not . null) (res:adds)
               then Just $ Puzzle adds res
               else Nothing
        _ -> Nothing

-- | Converts a word to a number based on the given letter-to-digit assignment.
-- Returns Nothing if any letter in the word is not in the assignment.
wordToNum :: [(Char, Int)] -> String -> Maybe Integer
wordToNum assignment word = foldl calc (Just 0) word
  where
    calc maybeAcc char = do
        acc <- maybeAcc
        digit <- lookup char assignment
        Just (acc * 10 + toInteger digit)

-- | Checks if an assignment is valid for the given puzzle.
isValid :: Puzzle -> [(Char, Int)] -> Bool
isValid puzzle assignment =
    -- Check leading zero constraint for multi-digit words
    all (\w -> maybe True (/= 0) (lookup (head w) assignment)) (filter ((>1) . length) (result puzzle : addends puzzle)) &&
    -- Check the arithmetic equation
    case mapM (wordToNum assignment) (addends puzzle) of
        Just addNums -> case wordToNum assignment (result puzzle) of
            Just resNum -> sum addNums == resNum
            Nothing -> False -- Should not happen if all letters are assigned
        Nothing -> False -- Should not happen if all letters are assigned

-- | Solves the alphametics puzzle.
solve :: String -> Maybe [(Char, Int)]
solve puzzleStr = do
    puzzle <- parsePuzzle puzzleStr
    let allWords = result puzzle : addends puzzle
        letters = nub $ filter isLetter $ concat allWords
        -- No need to pass leadingLetters here anymore
    find (isValid puzzle) $ generateAssignments letters

-- | Generates possible assignments (permutations of digits for letters).
generateAssignments :: [Char] -> [[(Char, Int)]]
generateAssignments letters =
    let numLetters = length letters
    -- Directly generate assignments from permutations
    in map (zip letters . take numLetters) (permutations [0..9])

-- | Tries a specific permutation of digits for the letters.
-- This function is simplified as the leading zero check is fully handled in isValid.
-- We can actually inline this into generateAssignments. Let's keep it separate for now
-- but remove the incorrect check. Or better, let's inline it.

-- Removed tryPermutation and integrated its logic directly into generateAssignments.
-- The incorrect leading zero check is removed.

