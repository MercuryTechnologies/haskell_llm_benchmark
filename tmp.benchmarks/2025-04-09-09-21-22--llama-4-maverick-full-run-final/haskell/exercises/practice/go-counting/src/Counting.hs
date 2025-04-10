module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set, empty, fromList, insert, member, union, unions)
import qualified Data.Set as Set

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

isValidCoord :: [String] -> Coord -> Bool
isValidCoord board (x, y) = x > 0 && y > 0 && x <= length (head board) && y <= length board

getNeighbors :: [String] -> Coord -> [Coord]
getNeighbors board coord@(x, y) = filter (isValidCoord board) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

getColor :: [String] -> Coord -> Maybe Color
getColor board (x, y) = case (board !! (y-1)) !! (x-1) of
    'B' -> Just Black
    'W' -> Just White
    _ -> Nothing

getTerritory :: [String] -> Coord -> (Set Coord, Set Color)
getTerritory board coord = go empty (Set.singleton coord) empty
  where
    go :: Set Coord -> Set Coord -> Set Color -> (Set Coord, Set Color)
    go visited toVisit colors
        | Set.null toVisit = (visited, colors)
        | otherwise = let
            current = Set.findMin toVisit
            newToVisit = Set.delete current toVisit
            newVisited = Set.insert current visited
            newColors = case getColor board current of
                Just c -> Set.insert c colors
                Nothing -> colors
            newNeighbors = Set.fromList $ filter (`Set.notMember` visited) $ getNeighbors board current
            in go newVisited (Set.union newToVisit newNeighbors) newColors

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = map (\(coords, colors) -> (coords, if Set.size colors == 1 then Just (Set.findMin colors) else Nothing)) territories'
  where
    emptyCoords = [(x, y) | y <- [1..length board], x <- [1..length (head board)], (board !! (y-1)) !! (x-1) == ' ']
    territories' = map (getTerritory board) emptyCoords

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord@(x, y)
    | (board !! (y-1)) !! (x-1) /= ' ' = Nothing
    | otherwise = let (territory, colors) = getTerritory board coord
                  in Just (territory, if Set.size colors == 1 then Just (Set.findMin colors) else Nothing)
