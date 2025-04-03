module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set

type Coord = (Int, Int)

data Color = Black | White deriving (Eq, Ord, Show)

-- Get board dimensions
-- Now we treat the board as a list of columns.
-- Number of rows is the length of the first string,
-- number of columns is the length of the board.
boardDimensions :: [String] -> (Int, Int)
boardDimensions board = (if null board then 0 else length (head board), length board)

-- Convert board coordinate to char; coordinates are 1-based.
-- We now index into the board as board !! (col - 1) !! (row - 1)
boardAt :: [String] -> Coord -> Maybe Char
boardAt board (row, col) =
    let (rows, cols) = boardDimensions board
    in if row < 1 || row > rows || col < 1 || col > cols
       then Nothing
       else Just ((board !! (col - 1)) !! (row - 1))

-- Check if the board position is empty (a space character ' ')
isEmpty :: [String] -> Coord -> Bool
isEmpty board coord = case boardAt board coord of
    Just c -> c == ' '
    _ -> False

-- Map a character to a Color if appropriate.
charToColor :: Char -> Maybe Color
charToColor c = case c of
    'B' -> Just Black
    'W' -> Just White
    _   -> Nothing

-- Get neighbors coordinates (up, down, left, right) for a given coordinate.
neighbors :: Coord -> [Coord]
neighbors (r, c) = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

-- Flood fill function that from an empty starting coordinate collects the contiguous empty intersections
-- and gathers any adjacent stone colors encountered.
floodFill :: [String] -> Coord -> (Set Coord, Set Color)
floodFill board start = go (Set.singleton start) (Set.singleton start) Set.empty
  where
    go :: Set Coord -> Set Coord -> Set Color -> (Set Coord, Set Color)
    go frontier visited stones =
      if Set.null frontier then (visited, stones)
      else
        let (nextFrontier, newVisited, newStones) = Set.foldr process (Set.empty, visited, stones) frontier
        in go nextFrontier newVisited newStones

    process :: Coord -> (Set Coord, Set Coord, Set Color) -> (Set Coord, Set Coord, Set Color)
    process coord (accFrontier, accVisited, accStones) =
      let ns = neighbors coord
          processNeighbor (fAcc, vAcc, sAcc) nbr =
            case boardAt board nbr of
              Nothing -> (fAcc, vAcc, sAcc)  -- out of bounds
              Just ch ->
                if ch == ' ' then
                  if Set.member nbr vAcc then (fAcc, vAcc, sAcc)
                  else (Set.insert nbr fAcc, Set.insert nbr vAcc, sAcc)
                else case charToColor ch of
                       Just color -> (fAcc, vAcc, Set.insert color sAcc)
                       Nothing -> (fAcc, vAcc, sAcc)
      in foldl processNeighbor (accFrontier, accVisited, accStones) ns

determineOwner :: Set Color -> Maybe Color
determineOwner colors
  | Set.size colors == 1 = Just (Set.findMin colors)
  | otherwise = Nothing

-- Returns the list of territories found in the board.
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = go allCoords Set.empty []
  where
    (rows, cols) = boardDimensions board
    allCoords = [(r, c) | r <- [1..rows], c <- [1..cols]]
    go [] _ acc = acc
    go (coord:coords) seen acc =
      if not (isEmpty board coord) || Set.member coord seen then go coords seen acc
      else
        let (region, stoneSet) = floodFill board coord
            owner = determineOwner stoneSet
            newSeen = Set.union seen region
        in go coords newSeen ((region, owner) : acc)

-- Returns the territory that contains the given coordinate, if it's an empty intersection.
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord =
  if not (isEmpty board coord) then Nothing
  else
    let (region, stoneSet) = floodFill board coord
        owner = determineOwner stoneSet
    in Just (region, owner)
