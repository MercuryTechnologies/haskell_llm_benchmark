module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos { col :: Int, row :: Int } deriving (Eq, Show)
data WordPos = WordPos { start :: CharPos, end :: CharPos } deriving (Eq, Show)

-- | Searches for each word in a grid puzzle.
-- For each word it returns the location of its first and last letter as a WordPos.
-- If a word is not found in the grid, Nothing is returned. 
search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\w -> (w, findWord grid w)) wordList

-- All eight possible directions: vertical, horizontal, and diagonal moves.
directions :: [(Int, Int)]
directions = [ (dr, dc) 
             | dr <- [-1,0,1]
             , dc <- [-1,0,1]
             , not (dr == 0 && dc == 0)
             ]

-- | Searches for a single word in the puzzle grid.
findWord :: [String] -> String -> Maybe WordPos
findWord grid word = 
  let numRows = length grid
      numCols = if null grid then 0 else length (head grid)
      -- Checks if starting at (r,c) and moving in (dr,dc) finds the word.
      checkDir :: Int -> Int -> (Int, Int) -> Maybe WordPos
      checkDir r c (dr, dc) =
        let len = length word
            positions = [(r + i * dr, c + i * dc) | i <- [0 .. len - 1]]
            inBounds (r', c') = r' >= 0 && r' < numRows && c' >= 0 && c' < numCols
            lettersMatch = all (\(i, (r', c')) -> inBounds (r', c') && (grid !! r') !! c' == word !! i)
                               (zip [0..] positions)
        in if lettersMatch 
             then let startPos = CharPos { col = c + 1, row = r + 1 }
                      endPos = CharPos { col = c + (len - 1) * dc + 1, row = r + (len - 1) * dr + 1 }
                  in Just (WordPos startPos endPos)
             else Nothing
      -- Search the grid: iterate row by row, column by column, and then each direction.
      searchFromCell :: Int -> Int -> Maybe WordPos
      searchFromCell r c = 
         foldr (\dir acc -> 
                  case acc of
                    Just _  -> acc
                    Nothing -> checkDir r c dir
               ) Nothing directions
  in foldr (\r accRow -> 
              case accRow of
                Just pos -> Just pos
                Nothing -> foldr (\c innerAcc -> 
                                    case innerAcc of
                                      Just pos -> Just pos
                                      Nothing  -> searchFromCell r c
                                  ) Nothing [0 .. numCols - 1]
           ) Nothing [0 .. numRows - 1]
