module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (nub)
import Prelude hiding (getChar)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

-- Get the character at a coordinate, or Nothing if out of bounds
getChar :: [String] -> Coord -> Maybe Char
getChar board (x, y)
    | y < 1 || y > length board = Nothing
    | x < 1 || x > length (head board) = Nothing
    | otherwise = Just ((board !! (y - 1)) !! (x - 1))

-- Get neighbors of a coordinate
neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

-- Flood fill from a starting coordinate, collecting empty points and bordering colors
floodFill :: [String] -> Coord -> (Set Coord, Set Color)
floodFill board start = go Set.empty Set.empty [start]
  where
    go visited colors [] = (visited, colors)
    go visited colors (c:cs)
        | c `Set.member` visited = go visited colors cs
        | otherwise =
            case getChar board c of
                Nothing -> go visited colors cs
                Just ' ' -> go (Set.insert c visited) colors (neighbors c ++ cs)
                Just 'B' -> go visited (Set.insert Black colors) cs
                Just 'W' -> go visited (Set.insert White colors) cs
                _        -> go visited colors cs

-- Determine the owner based on bordering colors
owner :: Set Color -> Maybe Color
owner colors
    | Set.size colors == 1 = Just (Set.findMin colors)
    | otherwise = Nothing

-- Return the territory for a given coordinate, if it's empty
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord =
    case getChar board coord of
        Just ' ' ->
            let (coords, colors) = floodFill board coord
            in Just (coords, owner colors)
        _ -> Nothing

-- Return all territories on the board
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = go Set.empty allCoords []
  where
    height = length board
    width = if null board then 0 else length (head board)
    allCoords = [(x,y) | y <- [1..height], x <- [1..width]]

    go _ [] acc = acc
    go visited (c:cs) acc
        | c `Set.member` visited = go visited cs acc
        | otherwise =
            case getChar board c of
                Just ' ' ->
                    let (coords, colors) = floodFill board c
                        newVisited = visited `Set.union` coords
                        terrOwner = owner colors
                    in go newVisited cs ((coords, terrOwner) : acc)
                _ -> go visited cs acc
