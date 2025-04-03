module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

-- Helper function to check if a coordinate is within board boundaries
inBounds :: [String] -> Coord -> Bool
inBounds board (row, col) = 
    row >= 1 && row <= length board && col >= 1 && col <= length (head board)

-- Helper function to get the character at a coordinate (1-based indexing)
charAt :: [String] -> Coord -> Char
charAt board (row, col) = (board !! (row-1)) !! (col-1)

-- Helper function to check if a coordinate is empty
isEmpty :: [String] -> Coord -> Bool
isEmpty board coord = charAt board coord == ' '

-- Helper function to get the color of a stone
stoneColor :: Char -> Maybe Color
stoneColor 'B' = Just Black
stoneColor 'W' = Just White
stoneColor _ = Nothing

-- Helper function to get adjacent coordinates
adjacentCoords :: Coord -> [Coord]
adjacentCoords (row, col) = [(row-1, col), (row+1, col), (row, col-1), (row, col+1)]

-- Helper function to get valid adjacent coordinates
validAdjacent :: [String] -> Coord -> [Coord]
validAdjacent board coord = filter (inBounds board) (adjacentCoords coord)

-- Helper function to find a territory starting from a coordinate using flood-fill
findTerritory :: [String] -> Coord -> Set Coord -> Set Coord
findTerritory board coord visited
    | not (inBounds board coord) = visited
    | not (isEmpty board coord) = visited
    | Set.member coord visited = visited
    | otherwise = 
        let newVisited = Set.insert coord visited
            adjacents = filter (isEmpty board) (validAdjacent board coord)
        in foldl' (\acc c -> findTerritory board c acc) newVisited adjacents

-- Helper function to determine the owner of a territory
determineOwner :: [String] -> Set Coord -> Maybe Color
determineOwner board territory =
    let allAdjacents = Set.fromList $ concatMap (validAdjacent board) (Set.toList territory)
        borderCoords = Set.filter (not . isEmpty board) (Set.difference allAdjacents territory)
        borderColors = map (stoneColor . charAt board) (Set.toList borderCoords)
    in if all (== Just Black) borderColors && not (null borderColors)
       then Just Black
       else if all (== Just White) borderColors && not (null borderColors)
            then Just White
            else Nothing

-- Main function to find all territories on the board
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = 
    let allCoords = [(row, col) | row <- [1..length board], col <- [1..length (head board)]]
        emptyCoords = filter (isEmpty board) allCoords
        -- Fixed the findTerritories function to use the visited parameter correctly
        findTerritories acc _ [] = acc
        findTerritories acc visited (c:cs)
            | Set.member c visited = findTerritories acc visited cs
            | otherwise = 
                let territory = findTerritory board c Set.empty
                    owner = determineOwner board territory
                    newVisited = Set.union visited territory
                in findTerritories ((territory, owner):acc) newVisited cs
    in findTerritories [] Set.empty emptyCoords

-- Function to find the territory containing a specific coordinate
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord
    | not (inBounds board coord) = Nothing
    | not (isEmpty board coord) = Nothing
    | otherwise = 
        let territory = findTerritory board coord Set.empty
            owner = determineOwner board territory
        in Just (territory, owner)
