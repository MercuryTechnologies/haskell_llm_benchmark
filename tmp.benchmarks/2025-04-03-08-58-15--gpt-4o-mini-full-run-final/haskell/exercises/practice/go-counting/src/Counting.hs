module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set, fromList, toList, empty, insert, member)
import qualified Data.Set as Set
import Data.List (nub)
import Data.Maybe (isNothing)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = nub [territory | row <- [0..length board - 1], col <- [0..length (board !! row) - 1], 
    let coord = (row + 1, col + 1), 
    isEmpty coord board, 
    let territory = exploreTerritory coord board]
  where
    exploreTerritory coord board = 
      let (coords, owner) = floodFill coord board Set.empty in
      (coords, owner)

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord
  | isEmpty coord board = Just (exploreTerritory coord board Set.empty)
  | otherwise = Nothing
  where
    exploreTerritory coord board visited =
      let (newCoords, owner) = floodFill coord board visited in
      (newCoords, owner)

isEmpty :: Coord -> [String] -> Bool
isEmpty (x, y) board = 
  x > 0 && y > 0 && x <= length board && y <= length (board !! (x - 1)) && 
  (board !! (x - 1) !! (y - 1) == ' ')

floodFill :: Coord -> [String] -> Set Coord -> (Set Coord, Maybe Color)
floodFill coord board visited
  | member coord visited = (visited, Nothing)
  | otherwise = 
      let newVisited = Set.insert coord visited
          neighbors = getNeighbors coord
          (newVisited', owner) = foldr (\n (v, o) -> 
              case getOwner n board of
                Just c -> (v, Just c)
                Nothing -> floodFill n board v) (newVisited, Nothing) neighbors
      in (newVisited', if isNothing owner then Nothing else owner)

getNeighbors :: Coord -> [Coord]
getNeighbors (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

getOwner :: Coord -> [String] -> Maybe Color
getOwner (x, y) board
  | x > 0 && y > 0 && x <= length board && y <= length (board !! (x - 1)) =
      case board !! (x - 1) !! (y - 1) of
        'B' -> Just Black
        'W' -> Just White
        _   -> Nothing
  | otherwise = Nothing
