module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes, mapMaybe)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = catMaybes $ map (territoryFor board) emptyCoords
  where
    emptyCoords = [(x, y) | x <- [1..length board], y <- [1..length (head board)], (board !! (x-1)) !! (y-1) == ' ']

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord
    | not (isValidCoord board coord) || not (isEmpty board coord) = Nothing
    | otherwise = Just (territory, owner)
  where
    territory = exploreTerritory board coord Set.empty
    owner = determineOwner board territory

isValidCoord :: [String] -> Coord -> Bool
isValidCoord board (x, y) = x > 0 && y > 0 && x <= length board && y <= length (head board)

isEmpty :: [String] -> Coord -> Bool
isEmpty board (x, y) = isValidCoord board (x, y) && (board !! (x-1)) !! (y-1) == ' '

exploreTerritory :: [String] -> Coord -> Set Coord -> Set Coord
exploreTerritory board coord visited
    | Set.member coord visited = visited
    | otherwise = foldr (exploreTerritory board) (Set.insert coord visited) neighbors
  where
    neighbors = filter (isEmpty board) $ adjacentCoords coord

adjacentCoords :: Coord -> [Coord]
adjacentCoords (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

determineOwner :: [String] -> Set Coord -> Maybe Color
determineOwner board territory
    | null owners = Nothing
    | length owners == 1 = Just (head owners)
    | otherwise = Nothing
  where
    owners = Set.toList $ Set.fromList $ concatMap (adjacentOwners board) (Set.toList territory)

adjacentOwners :: [String] -> Coord -> [Color]
adjacentOwners board coord = mapMaybe (stoneOwner board) $ filter (isValidCoord board) $ adjacentCoords coord

stoneOwner :: [String] -> Coord -> Maybe Color
stoneOwner board (x, y) = case (board !! (x-1)) !! (y-1) of
    'B' -> Just Black
    'W' -> Just White
    _   -> Nothing
