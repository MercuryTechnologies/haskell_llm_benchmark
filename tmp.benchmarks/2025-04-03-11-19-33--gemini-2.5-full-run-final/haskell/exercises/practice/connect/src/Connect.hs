module Connect (Mark(..), winner) where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (findIndices)
import Data.Maybe (listToMaybe)

data Mark = Cross | Nought deriving (Eq, Show)

-- Represents coordinates (row, column)
type Coord = (Int, Int)

-- Safely get character at a coordinate, returns Nothing for out-of-bounds or space.
getCharAt :: [String] -> Coord -> Maybe Char
getCharAt board (r, c)
  | r < 0 || r >= numRows = Nothing -- Row out of bounds
  | c < 0 || c >= rowLen = Nothing  -- Column out of bounds for this row
  | otherwise = let char = board !! r !! c in if char == ' ' then Nothing else Just char
  where
    numRows = length board
    -- Get row length safely, return 0 if row index is invalid
    rowLen = if r >= 0 && r < numRows then length (board !! r) else 0

-- Get potential neighbors of a coordinate based on Hex grid adjacency.
neighbors :: Coord -> [Coord]
neighbors (r, c) =
  [ (r, c - 1), (r, c + 1)    -- Left, Right
  , (r - 1, c), (r - 1, c + 1) -- Top-Left, Top-Right
  , (r + 1, c), (r + 1, c - 1) -- Bottom-Right, Bottom-Left
  ]

-- Breadth-First Search to find if a path exists for the given mark
-- from any starting coordinate to a coordinate satisfying the target condition.
bfs :: Mark -> [String] -> (Coord -> Bool) -> Set Coord -> [Coord] -> Bool
bfs _ _ _ _ [] = False -- Queue empty, target not reached
bfs mark board isTarget visited (current:queue) =
    -- Check if the current coordinate has already been visited
    if current `Set.member` visited
    then bfs mark board isTarget visited queue -- Skip if visited
    else
        let playerChar = case mark of Cross -> 'X'; Nought -> 'O'
        in -- Check if the current coordinate is valid and belongs to the player
           case getCharAt board current of
             Just char | char == playerChar -> -- Cell belongs to the current player
                 -- Check if this cell satisfies the target condition (reached the destination edge)
                 if isTarget current
                 then True -- Winning path found
                 else
                     -- Mark current cell as visited
                     let visited' = Set.insert current visited
                         -- Find valid, unvisited neighbors belonging to the same player
                         validNeighbors =
                           [ n | n <- neighbors current             -- Get all potential neighbors
                               , getCharAt board n == Just playerChar -- Check neighbor has player's mark
                               , n `Set.notMember` visited'      -- Check neighbor not already visited
                           ]
                         -- Add valid neighbors to the end of the queue for BFS
                         queue' = queue ++ validNeighbors
                     -- Continue BFS with updated visited set and queue
                     in bfs mark board isTarget visited' queue'
             _ -> -- Cell is empty, belongs to the opponent, or is invalid; skip it
                  bfs mark board isTarget visited queue

-- Helper to find the index of the first non-space character in a string.
firstNonSpaceIndex :: String -> Maybe Int
firstNonSpaceIndex = listToMaybe . findIndices (/= ' ')

-- Helper to find the index of the last non-space character in a string.
lastNonSpaceIndex :: String -> Maybe Int
lastNonSpaceIndex s = fmap (\idx -> length s - 1 - idx) . listToMaybe . findIndices (/= ' ') $ reverse s

-- Determine the winner of the Hex game based on the board state.
winner :: [String] -> Maybe Mark
winner board
  -- Handle edge cases: empty board, rows are empty strings, or board contains only spaces.
  | null board || all null board || all (all (== ' ')) board = Nothing
  | otherwise =
      let numRows = length board
          -- Find the maximum length of any row string to check starting points safely.
          maxLength = maximum (0 : map length board)

          -- 1. Check for 'O' win (connects Top edge to Bottom edge)
          -- Starting coordinates for 'O' are any 'O' in the first row (r=0).
          oStarts = [ (0, c) | c <- [0..maxLength-1], getCharAt board (0, c) == Just 'O' ]
          -- Target condition for 'O' is reaching the last row.
          oTarget (r, _) = r == numRows - 1
          -- Run BFS for 'O' only if there are potential starting points.
          oWins = not (null oStarts) && bfs Nought board oTarget Set.empty oStarts

          -- 2. If 'O' did not win, check for 'X' win (connects Left edge to Right edge)
          -- Starting coordinates for 'X' are any 'X' at the first non-space position in any row.
          xStarts = concatMap findXStartForRow [0..numRows-1]
          findXStartForRow r =
              case firstNonSpaceIndex (board !! r) of
                -- If the first non-space char is 'X', it's a start coord.
                Just cFirst | getCharAt board (r, cFirst) == Just 'X' -> [(r, cFirst)]
                _ -> [] -- No 'X' at the start of this row or row is empty/all spaces.

          -- Target condition for 'X' is reaching the last non-space position in any row.
          xTarget (r, c) =
              case lastNonSpaceIndex (board !! r) of
                -- Check if the current coordinate 'c' is the last non-space char index.
                Just cLast -> c == cLast
                Nothing -> False -- Row is all spaces, cannot be a target.

          -- Run BFS for 'X' only if there are potential starting points.
          xWins = not (null xStarts) && bfs Cross board xTarget Set.empty xStarts

      -- Return the winner based on the BFS results.
      in if oWins then Just Nought
         else if xWins then Just Cross
         else Nothing
