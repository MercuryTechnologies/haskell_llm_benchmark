module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set, fromList, member, empty, insert)
import Data.Map (Map, fromList, (!?), toList)
import qualified Data.Map as M

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

-- Helper function to get the dimensions of the board
boardDimensions :: [String] -> (Int, Int)
boardDimensions board = (length board, length (head board))

-- Helper function to get the color of a stone at a given coordinate
stoneColor :: [String] -> Coord -> Maybe Color
stoneColor board (row, col)
    | row < 1 || row > numRows || col < 1 || col > numCols = Nothing
    | c == 'B' = Just Black
    | c == 'W' = Just White
    | otherwise = Nothing
  where
    (numRows, numCols) = boardDimensions board
    c = board !! (row - 1) !! (col - 1)

-- Helper function to get the neighbors of a coordinate
getNeighbors :: Coord -> (Int, Int) -> [Coord]
getNeighbors (row, col) (numRows, numCols) =
    filter (\(r, c) -> r >= 1 && r <= numRows && c >= 1 && c <= numCols)
    [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]

-- Helper function to find a territory
findTerritory :: [String] -> Coord -> Set Coord -> Maybe Color -> (Set Coord, Maybe Color)
findTerritory board coord visited currentColor =
  case stoneColor board coord of
    Just _ -> (empty, Nothing)  -- Found a stone, not part of a territory
    Nothing ->
      if member coord visited then
        (empty, currentColor)  -- Already visited, return current color
      else
        let
          (numRows, numCols) = boardDimensions board
          neighbors = getNeighbors coord (numRows, numCols)
          newVisited = insert coord visited
          territoryResults = map (findTerritory board) neighbors
          territoryColors = map (\(t, c) -> c) $ filter (\(t, c) -> t == empty) $ zip neighbors $ map (findTerritory board neighbor visited) neighbors
          territoryCoords = map (\(t, c) -> t) $ filter (\(t, c) -> t /= empty) $ zip neighbors $ map (findTerritory board neighbor visited) neighbors
          
          -- Determine the color of the territory
          newColor = case territoryColors of
            [] -> Nothing  -- No neighboring stones
            (c:cs) -> if all (== c) cs then Just c else Nothing  -- All neighboring stones are the same color, or mixed
          
          -- Combine the territories
          combinedTerritory = foldl insert (Data.Set.fromList [coord]) (concatMap toList territoryCoords)
        in
          (combinedTerritory, newColor)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board =
  let
    (numRows, numCols) = boardDimensions board
    allCoords = [(r, c) | r <- [1..numRows], c <- [1..numCols]]
    emptyCoords = filter (\coord -> stoneColor board coord == Nothing) allCoords
    
    -- Find territories for all empty coordinates
    territoryResults = map (findTerritory board empty) emptyCoords
    
    -- Filter out empty territories and duplicates
    filteredTerritories = removeDuplicateTerritories territoryResults
  in
    filteredTerritories

-- Helper function to remove duplicate territories
removeDuplicateTerritories :: [(Set Coord, Maybe Color)] -> [(Set Coord, Maybe Color)]
removeDuplicateTerritories [] = []
removeDuplicateTerritories (x:xs) =
  x : removeDuplicateTerritories (filter (\(territory, _) -> territory /= fst x) xs)

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord =
  case stoneColor board coord of
    Just _ -> Nothing  -- Not an empty location
    Nothing ->
      let
        (territory, color) = findTerritory board coord empty Nothing
      in
        if null territory then
          Nothing
        else
          Just (territory, color)
