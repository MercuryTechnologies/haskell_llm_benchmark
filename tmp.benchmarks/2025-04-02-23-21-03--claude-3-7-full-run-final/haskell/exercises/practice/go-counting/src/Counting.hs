module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (isNothing, catMaybes)
import Data.List (foldl')

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

-- | Find all territories on the board
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = 
    let height = length board
        width = if height > 0 then length (head board) else 0
        allCoords = [(r, c) | r <- [1..height], c <- [1..width]]
        emptyCoords = filter (isEmpty board) allCoords
    in findAllTerritories board emptyCoords Set.empty

-- | Find the territory containing the given coordinate
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord
    | not (isValidCoord board coord) = Nothing
    | not (isEmpty board coord) = Nothing
    | otherwise = 
        let (territory, owner) = findTerritory board coord Set.empty
        in Just (territory, owner)

-- | Find all territories starting from a list of empty coordinates
findAllTerritories :: [String] -> [Coord] -> Set Coord -> [(Set Coord, Maybe Color)]
findAllTerritories _ [] _ = []
findAllTerritories board (coord:rest) visited
    | Set.member coord visited = findAllTerritories board rest visited
    | otherwise =
        let (territory, owner) = findTerritory board coord Set.empty
            newVisited = Set.union visited territory
        in (territory, owner) : findAllTerritories board rest newVisited

-- | Find a single territory and its owner starting from a coordinate
findTerritory :: [String] -> Coord -> Set Coord -> (Set Coord, Maybe Color)
findTerritory board start visited = bfs board [start] visited Set.empty

-- | Breadth-first search to find all connected empty intersections
bfs :: [String] -> [Coord] -> Set Coord -> Set Color -> (Set Coord, Maybe Color)
bfs _ [] visited adjacentColors = (visited, determineOwner adjacentColors)
bfs board (coord:queue) visited adjacentColors
    | Set.member coord visited = bfs board queue visited adjacentColors
    | not (isEmpty board coord) = bfs board queue visited adjacentColors
    | otherwise =
        let visited' = Set.insert coord visited
            neighbors = getNeighbors board coord
            emptyNeighbors = filter (isEmpty board) neighbors
            newQueue = queue ++ filter (`Set.notMember` visited') emptyNeighbors
            stoneNeighbors = filter (not . isEmpty board) neighbors
            newAdjacentColors = foldl' (\acc c -> Set.insert (getColor board c) acc) 
                                adjacentColors stoneNeighbors
        in bfs board newQueue visited' newAdjacentColors

-- | Determine the owner of a territory based on adjacent stones
determineOwner :: Set Color -> Maybe Color
determineOwner colors
    | Set.size colors == 0 = Nothing  -- No adjacent stones
    | Set.size colors == 1 = Just (head (Set.toList colors))  -- One player's stones
    | otherwise = Nothing  -- Both players' stones

-- | Get the color at a coordinate
getColor :: [String] -> Coord -> Color
getColor board (r, c)
    | getBoardChar board (r, c) == 'B' = Black
    | getBoardChar board (r, c) == 'W' = White
    | otherwise = error "Not a stone"

-- | Check if a coordinate is empty
isEmpty :: [String] -> Coord -> Bool
isEmpty board coord = getBoardChar board coord == ' '

-- | Get the character at a coordinate (1-indexed)
getBoardChar :: [String] -> Coord -> Char
getBoardChar board (r, c)
    | isValidCoord board (r, c) = (board !! (r-1)) !! (c-1)
    | otherwise = error "Invalid coordinate"

-- | Check if a coordinate is valid
isValidCoord :: [String] -> Coord -> Bool
isValidCoord board (r, c) =
    let height = length board
        width = if height > 0 then length (head board) else 0
    in r >= 1 && r <= height && c >= 1 && c <= width

-- | Get the valid neighboring coordinates
getNeighbors :: [String] -> Coord -> [Coord]
getNeighbors board (r, c) =
    let candidates = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
    in filter (isValidCoord board) candidates
