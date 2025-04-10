module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

-- Helper: get the board dimensions
boardDims :: [String] -> (Int, Int)
boardDims board = (length board, if null board then 0 else length (head board))

-- Helper: get the character at a coordinate (1-based)
charAt :: [String] -> Coord -> Maybe Char
charAt board (r, c) =
    let (rows, cols) = boardDims board
    in if r >= 1 && r <= rows && c >= 1 && c <= cols
        then Just ((board !! (r - 1)) !! (c - 1))
        else Nothing

-- Helper: get all coordinates of the board
allCoords :: [String] -> [Coord]
allCoords board =
    let (rows, cols) = boardDims board
    in [(r, c) | r <- [1..rows], c <- [1..cols]]

-- Helper: get neighbors (up, down, left, right)
neighbors :: [String] -> Coord -> [Coord]
neighbors board (r, c) =
    let (rows, cols) = boardDims board
        ns = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
    in filter (\(r', c') -> r' >= 1 && r' <= rows && c' >= 1 && c' <= cols) ns

-- Helper: flood fill from a starting empty coordinate, returning the territory and bordering colors
floodFill :: [String] -> Coord -> (Set Coord, Set Color)
floodFill board start =
    go Set.empty Set.empty [start]
  where
    go visited borders [] = (visited, borders)
    go visited borders (x:xs)
        | x `Set.member` visited = go visited borders xs
        | otherwise =
            case charAt board x of
                Just ' ' ->
                    let ns = neighbors board x
                    in go (Set.insert x visited) borders (ns ++ xs)
                Just 'B' -> go visited (Set.insert Black borders) xs
                Just 'W' -> go visited (Set.insert White borders) xs
                _        -> go visited borders xs

-- Helper: determine the owner of a territory based on bordering colors
ownerOf :: Set Color -> Maybe Color
ownerOf borders
    | Set.size borders == 1 = Just (Set.findMin borders)
    | otherwise = Nothing

-- Main: find all territories
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = go Set.empty (allCoords board)
  where
    go _ [] = []
    go visited (coord:rest)
        | coord `Set.member` visited = go visited rest
        | otherwise =
            case charAt board coord of
                Just ' ' ->
                    let (territory, borders) = floodFill board coord
                        newVisited = Set.union visited territory
                    in (territory, ownerOf borders) : go newVisited rest
                _ -> go visited rest

-- Main: find the territory for a specific coordinate
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord =
    case charAt board coord of
        Just ' ' ->
            let (territory, borders) = floodFill board coord
            in Just (territory, ownerOf borders)
        _ -> Nothing
