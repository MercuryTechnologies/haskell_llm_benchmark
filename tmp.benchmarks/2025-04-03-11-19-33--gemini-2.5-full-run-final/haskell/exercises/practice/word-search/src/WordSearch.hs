module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.Maybe (listToMaybe, maybeToList)

-- Represents a position in the grid using 0-based column and row indices internally,
-- but the constructor expects 1-based indices for external representation as per tests.
-- The fields 'col' and 'row' will store the 1-based values.
data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)

-- Represents the start and end positions of a found word using 1-based CharPos.
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

-- Searches the grid for the words in wordList.
-- Returns a list associating each word with its found position (or Nothing).
-- The positions in WordPos use 1-based indexing.
search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\word -> (word, findWord grid word)) wordList

-- Tries to find a single word within the grid. Returns Maybe WordPos with 1-based coords.
findWord :: [String] -> String -> Maybe WordPos
findWord grid word
    -- Basic validation: cannot find empty word or search in empty grid.
    | null word || null grid || null (head grid) = Nothing
    | otherwise = listToMaybe $ do -- Use list monad to search; listToMaybe takes the first result.
        -- Internal logic uses 0-based indexing for rows (r) and columns (c).
        r <- [0 .. rows - 1]        -- Iterate through all possible starting rows (0-based).
        c <- [0 .. cols - 1]        -- Iterate through all possible starting columns (0-based).
        (dr, dc) <- directions      -- Iterate through all 8 directions.
        -- Create the starting CharPos using 0-based indices for internal checks.
        -- Note: This CharPos is temporary for checkWord; the final result will be 1-based.
        let internalStartPos = CharPos c r
        -- Attempt to match the word starting at (r, c) in direction (dr, dc).
        -- maybeToList converts Nothing to [] and Just x to [x].
        -- The list monad effectively concatenates all results, and listToMaybe picks the first.
        maybeToList (checkWord grid word internalStartPos (dr, dc))
  where
    rows = length grid
    cols = length (head grid)
    -- Define the 8 possible directions (row change, column change).
    directions = [(dr, dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1], (dr, dc) /= (0, 0)]

-- Checks if the given word can be formed starting at 'internalStartPos' (0-based)
-- in the direction specified by '(dr, dc)'.
-- Returns Maybe WordPos with 1-based coordinates if found.
checkWord :: [String] -> String -> CharPos -> (Int, Int) -> Maybe WordPos
-- Parameter renamed to internalStartPos to avoid shadowing 'start' field of WordPos
checkWord grid word internalStartPos@(CharPos startC startR) (dr, dc) =
    let
        wordLen = length word
        -- Calculate the theoretical 0-based end coordinates based on word length and direction.
        endR = startR + (wordLen - 1) * dr
        endC = startC + (wordLen - 1) * dc
        rows = length grid
        cols = length (head grid)

        -- Helper function to check if a 0-based coordinate (r, c) is within the grid boundaries.
        inBounds :: Int -> Int -> Bool
        inBounds r c = r >= 0 && r < rows && c >= 0 && c < cols

        -- Generate all 0-based coordinates the word would occupy if placed at startPos in the given direction.
        coords :: [(Int, Int)]
        coords = [(startR + i * dr, startC + i * dc) | i <- [0 .. wordLen - 1]]

        -- Check 1: Are all 0-based coordinates within the grid boundaries?
        allCoordsInBounds :: Bool
        allCoordsInBounds = all (uncurry inBounds) coords

        -- Check 2: Do the characters in the grid at these 0-based coordinates match the word?
        -- This check is only performed if all coordinates are valid.
        charactersMatch :: Bool
        charactersMatch = allCoordsInBounds && all checkChar (zip [0..] coords)
          where
            -- Check if the character in the grid at 0-based (r, c) matches the i-th character of the word.
            checkChar (i, (r, c)) = (grid !! r !! c) == (word !! i)

    -- If both checks pass, construct the WordPos.
    -- Convert the 0-based start (startC, startR) and end (endC, endR) coordinates
    -- to 1-based coordinates for the final WordPos result.
    in if charactersMatch
       then Just (WordPos (CharPos (startC + 1) (startR + 1)) -- Convert start to 1-based
                         (CharPos (endC + 1) (endR + 1)))   -- Convert end to 1-based
       else Nothing -- Otherwise, the word doesn't match in this position/direction.
