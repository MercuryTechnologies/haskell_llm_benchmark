module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

-- Get character at coordinate (1-based)
getAt :: [String] -> Coord -> Maybe Char
getAt board (x, y)
    | x < 1 || y < 1 = Nothing
    | x > length board || y > length (head board) = Nothing
    | otherwise = Just (board !! (x-1) !! (y-1))

-- Get all adjacent coordinates (up, down, left, right)
adjacentCoords :: Coord -> [Coord]
adjacentCoords (x, y) = [(x,y-1), (x,y+1), (x-1,y), (x+1,y)]

-- Find all empty coordinates on the board
emptyCoords :: [String] -> [Coord]
emptyCoords board = 
    [(x, y) | x <- [1..length board], 
              y <- [1..length (head board)], 
              getAt board (x, y) == Just ' ']

-- Flood fill to find connected empty spaces starting from a coordinate
findTerritory :: [String] -> Coord -> Set Coord -> Set Coord
findTerritory board coord visited
    | Set.member coord visited = visited
    | getAt board coord /= Just ' ' = visited
    | otherwise = foldr (findTerritory board) (Set.insert coord visited) (adjacentCoords coord)

-- Get bordering colors for a territory
getBorderingColors :: [String] -> Set Coord -> [Color]
getBorderingColors board territory = catMaybes $ concatMap checkAdjacent (Set.toList territory)
    where
        checkAdjacent coord = map (getColor . getAt board) (adjacentCoords coord)
        getColor (Just 'B') = Just Black
        getColor (Just 'W') = Just White
        getColor _ = Nothing

-- Determine territory owner based on bordering colors
determineOwner :: [Color] -> Maybe Color
determineOwner colors
    | null colors = Nothing
    | all (== Black) colors = Just Black
    | all (== White) colors = Just White
    | otherwise = Nothing

-- Main function to find all territories
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = 
    let empties = emptyCoords board
        findTerritories [] _ = []
        findTerritories (e:es) visited
            | Set.member e visited = findTerritories es visited
            | otherwise = 
                let territory = findTerritory board e Set.empty
                    colors = getBorderingColors board territory
                    owner = determineOwner colors
                in (territory, owner) : findTerritories es (Set.union visited territory)
    in findTerritories empties Set.empty

-- Find territory containing specific coordinate
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord = 
    case getAt board coord of
        Just ' ' -> 
            let territory = findTerritory board coord Set.empty
                colors = getBorderingColors board territory
                owner = determineOwner colors
            in Just (territory, owner)
        _ -> Nothing
