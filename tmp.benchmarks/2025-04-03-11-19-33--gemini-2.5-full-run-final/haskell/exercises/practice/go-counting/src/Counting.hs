module Counting (
    Color(..),
    territories,
    territoryFor
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (foldl')

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int) -- 1-based (col, row) for output
type Coord0 = (Int, Int) -- 0-based (row, col) for internal processing

-- Represents the board internally
type Board = Map.Map Coord0 Char
type BoardInfo = (Board, Int, Int) -- Board map, width, height

-- Parses the input string list into a map and dimensions
parseBoard :: [String] -> BoardInfo
parseBoard rows = (boardMap, width, height)
  where
    height = length rows
    width = if height == 0 then 0 else length (head rows)
    coords = [((r, c), cell) | (r, row) <- zip [0..] rows, (c, cell) <- zip [0..] row]
    boardMap = Map.fromList coords

-- Get valid neighbors (up, down, left, right) for a 0-based coordinate
neighbors :: Int -> Int -> Coord0 -> [Coord0]
neighbors width height (r, c) =
    filter isValid [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
  where
    isValid (nr, nc) = nr >= 0 && nr < height && nc >= 0 && nc < width

-- Convert 0-based (row, col) to 1-based (col, row)
toCoord1 :: Coord0 -> Coord
toCoord1 (r, c) = (c + 1, r + 1)

-- Convert 1-based (col, row) to 0-based (row, col)
toCoord0 :: Coord -> Coord0
toCoord0 (x, y) = (y - 1, x - 1)

-- Flood fill (BFS) to find connected empty region and adjacent stone colors
-- Returns (territory coordinates, adjacent stone colors, all visited coordinates in this search)
floodFill :: BoardInfo -> Set.Set Coord0 -> Coord0 -> (Set.Set Coord0, Set.Set Color, Set.Set Coord0)
floodFill (board, width, height) globalVisited startCoord =
    bfs Set.empty Set.empty (Set.singleton startCoord) [startCoord]
  where
    bfs territory adjacentColors visited queue =
        case queue of
            [] -> (territory, adjacentColors, visited)
            (current : restQueue) ->
                let currentNeighbors = neighbors width height current
                    (newTerritory, newAdjacent, newVisited, newQueue) =
                        foldl' (processNeighbor current) (territory, adjacentColors, visited, []) currentNeighbors
                in bfs (Set.insert current newTerritory) newAdjacent newVisited (restQueue ++ newQueue)

    processNeighbor current (terr, adj, vstd, nq) neighborCoord =
        case Map.lookup neighborCoord board of
            Just ' ' -> -- Empty neighbor
                if Set.member neighborCoord vstd || Set.member neighborCoord globalVisited
                then (terr, adj, vstd, nq) -- Already visited globally or locally
                else (terr, adj, Set.insert neighborCoord vstd, neighborCoord : nq) -- Visit later
            Just 'B' -> (terr, Set.insert Black adj, vstd, nq) -- Black stone neighbor
            Just 'W' -> (terr, Set.insert White adj, vstd, nq) -- White stone neighbor
            _        -> (terr, adj, vstd, nq) -- Outside board or unexpected char

-- Determine owner based on adjacent colors
determineOwner :: Set.Set Color -> Maybe Color
determineOwner adjacentColors =
    case Set.toList adjacentColors of
        [c] -> Just c -- Exactly one color borders the territory
        _   -> Nothing -- Zero or both colors border it

-- Find all territories on the board
territories :: [String] -> [(Set.Set Coord, Maybe Color)]
territories boardStr = findTerritories boardInfo (Map.keysSet $ fst3 boardInfo) Set.empty []
  where
    boardInfo@(board, _, _) = parseBoard boardStr
    fst3 (a, _, _) = a

    findTerritories :: BoardInfo -> Set.Set Coord0 -> Set.Set Coord0 -> [(Set.Set Coord, Maybe Color)] -> [(Set.Set Coord, Maybe Color)]
    findTerritories bInfo remainingCoords visited acc =
        case Set.minView remainingCoords of
            Nothing -> acc -- No more coordinates to check
            Just (coord, restCoords) ->
                if Set.member coord visited || Map.lookup coord (fst3 bInfo) /= Just ' '
                then findTerritories bInfo restCoords visited acc -- Skip if visited or not empty
                else
                    let (territory0, adjacentColors, newlyVisited) = floodFill bInfo visited coord
                        owner = determineOwner adjacentColors
                        territory1 = Set.map toCoord1 territory0
                        newVisited = Set.union visited newlyVisited
                        newRemaining = Set.difference restCoords newVisited -- Remove newly visited from remaining
                    in findTerritories bInfo newRemaining newVisited ((territory1, owner) : acc)

-- Find the territory containing a specific coordinate
territoryFor :: [String] -> Coord -> Maybe (Set.Set Coord, Maybe Color)
territoryFor boardStr coord1 =
    let boardInfo@(board, width, height) = parseBoard boardStr
        coord0 = toCoord0 coord1
        (r, c) = coord0
    in -- Check if coord0 is valid and empty
       if r < 0 || r >= height || c < 0 || c >= width || Map.lookup coord0 board /= Just ' '
       then Nothing
       else
           let (territory0, adjacentColors, _) = floodFill boardInfo Set.empty coord0
               owner = determineOwner adjacentColors
               territory1 = Set.map toCoord1 territory0
           in Just (territory1, owner)

