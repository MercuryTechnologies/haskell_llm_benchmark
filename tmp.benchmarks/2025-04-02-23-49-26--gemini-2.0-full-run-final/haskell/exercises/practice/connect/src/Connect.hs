import Data.List (elemIndex)
import Data.Maybe (isJust)

module Connect (Mark(..), winner) where

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner board =
  if null board
    then Nothing
    else case (hasWon Nought board, hasWon Cross board) of
      (True, _) -> Just Nought
      (_, True) -> Just Cross
      (False, False) -> Nothing

hasWon :: Mark -> [String] -> Bool
hasWon mark board =
  if null board
    then False
    else case mark of
      Nought -> any (startSearch Nought board) [0 .. (getWidth board - 1)]
      Cross -> any (startSearch Cross (transpose board)) [0 .. (getHeight board - 1)]

startSearch :: Mark -> [String] -> Int -> Bool
startSearch mark board start =
  let height = getHeight board
      width = getWidth board
  in dfs mark board (0, start) []
  where
    dfs :: Mark -> [String] -> (Int, Int) -> [(Int, Int)] -> Bool
    dfs mark board (row, col) visited
      | row < 0 || row >= height || col < 0 || col >= width = False
      | (row, col) `elem` visited = False
      | getMarkAt board (row, col) /= Just mark = False
      | mark == Nought && row == height - 1 = True
      | mark == Cross && col == width - 1 = True
      | otherwise =
          let newVisited = (row, col) : visited
          in any
              (\(dr, dc) ->
                dfs mark board (row + dr, col + dc) newVisited
              )
              [(0, 1), (0, -1), (1, 0), (-1, 0), (1, -1), (-1, 1)]

getMarkAt :: [String] -> (Int, Int) -> Maybe Mark
getMarkAt board (row, col)
  | row < 0 || row >= length board = Nothing
  | col < 0 || col >= length (board !! row) = Nothing
  | board !! row !! col == 'O' = Just Nought
  | board !! row !! col == 'X' = Just Cross
  | otherwise = Nothing

transpose :: [String] -> [String]
transpose ([] :: [String]) = []
transpose ([] : _) = []
transpose rows =
  [ [ rows !! i !! j | i <- [0 .. length rows - 1] ] | j <- [0 .. length (head rows) - 1] ]

getHeight :: [String] -> Int
getHeight board = length board

getWidth :: [String] -> Int
getWidth board = if null board then 0 else length (head board)
