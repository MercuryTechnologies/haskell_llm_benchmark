module Connect (Mark(..), winner) where

import Data.List (nub)
import Data.Maybe (isJust, listToMaybe, mapMaybe)

data Mark = Cross | Nought deriving (Eq, Show)

type Position = (Int, Int)
type Board = [String]

-- Check if a player has won by connecting their sides
winner :: [String] -> Maybe Mark
winner board
  | hasWon Nought board = Just Nought
  | hasWon Cross board = Just Cross
  | otherwise = Nothing

-- Check if a specific player has won
hasWon :: Mark -> Board -> Bool
hasWon mark board = any (isJust . listToMaybe) [findPath mark board start | start <- startingPositions mark board]

-- Get all starting positions for a player
startingPositions :: Mark -> Board -> [Position]
startingPositions mark board = case mark of
  Nought -> [(0, c) | c <- [0..width-1], getMark board (0, c) == Just Nought]
  Cross -> [(r, 0) | r <- [0..height-1], getMark board (r, 0) == Just Cross]
  where
    width = if null board then 0 else length (head board)
    height = length board

-- Get the mark at a position
getMark :: Board -> Position -> Maybe Mark
getMark board (r, c)
  | r < 0 || r >= length board = Nothing
  | c < 0 || c >= rowLength = Nothing
  | otherwise = case (board !! r) !! c of
      'X' -> Just Cross
      'O' -> Just Nought
      _ -> Nothing
  where rowLength = if null board then 0 else length (head board)

-- Find a path from start to opposite side using DFS
findPath :: Mark -> Board -> Position -> Maybe [Position]
findPath mark board start = dfs mark board [start] start

-- Depth-first search to find path to opposite side
dfs :: Mark -> Board -> [Position] -> Position -> Maybe [Position]
dfs mark board visited (r, c)
  | isOppositeSide mark (r, c) (length board) (if null board then 0 else length (head board)) = Just visited
  | otherwise = listToMaybe $ mapMaybe (dfs mark board (newPos:visited)) neighbors
  where
    neighbors = filter (isConnected mark board) $ getNeighbors (r, c)
    newPos = (r, c)
    isConnected m b pos = getMark b pos == Just m && pos `notElem` visited

-- Get all valid neighbor positions
getNeighbors :: Position -> [Position]
getNeighbors (r, c) =
  [(r-1, c), (r-1, c+1), (r, c-1), (r, c+1), (r+1, c-1), (r+1, c)]

-- Check if position is on opposite side for the player
isOppositeSide :: Mark -> Position -> Int -> Int -> Bool
isOppositeSide mark (r, c) height width = case mark of
  Nought -> r == height - 1
  Cross -> c == width - 1
