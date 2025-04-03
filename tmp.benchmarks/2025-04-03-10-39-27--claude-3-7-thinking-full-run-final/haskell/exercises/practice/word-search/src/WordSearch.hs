module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

-- Define the 8 possible directions for searching: (rowDelta, colDelta)
directions :: [(Int, Int)]
directions = [(0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)]

-- Check if a position is within the grid boundaries
inBounds :: Int -> Int -> CharPos -> Bool
inBounds rows cols (CharPos c r) = r >= 1 && r <= rows && c >= 1 && c <= cols

-- Get the character at a specific position in the grid
charAt :: [String] -> CharPos -> Char
charAt grid (CharPos c r) = (grid !! (r - 1)) !! (c - 1)

-- Check if a word can be found starting from a position in a specific direction
checkDirection :: [String] -> String -> CharPos -> (Int, Int) -> Maybe WordPos
checkDirection grid word startPos (dr, dc) =
  let rows = length grid
      cols = length (head grid)
      endPos = CharPos (col startPos + (length word - 1) * dc) (row startPos + (length word - 1) * dr)
      positions = [CharPos (col startPos + i * dc) (row startPos + i * dr) | i <- [0..length word - 1]]
      allValid = all (inBounds rows cols) positions
      matchesWord = allValid && [charAt grid pos | pos <- positions] == word
  in if matchesWord then Just (WordPos startPos endPos) else Nothing

-- Find a word in the grid
findWord :: [String] -> String -> Maybe WordPos
findWord grid word =
  let rows = length grid
      cols = length (head grid)
      possibleStarts = [CharPos c r | r <- [1..rows], c <- [1..cols], charAt grid (CharPos c r) == head word]
      results = [res | pos <- possibleStarts, dir <- directions, 
                    let res = checkDirection grid word pos dir, 
                    res /= Nothing]
  in case results of
       (r:_) -> r
       [] -> Nothing

-- Search for all words in the list
search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = [(word, findWord grid word) | word <- wordList]
