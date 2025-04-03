module Connect (Mark(..), winner) where

import Data.Maybe (isJust)
import qualified Data.Set as Set

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner board
    | hasWon Nought board = Just Nought
    | hasWon Cross board  = Just Cross
    | otherwise           = Nothing

hasWon :: Mark -> [String] -> Bool
hasWon mark board = 
    let startPoints = if mark == Nought then [(0, c) | c <- [0..(length (head board) - 1)], board !! 0 !! c == 'O']
                     else [(r, 0) | r <- [0..(length board - 1)], board !! r !! 0 == 'X']
    in any (dfs mark Set.empty board) startPoints

dfs :: Mark -> Set.Set (Int, Int) -> [String] -> (Int, Int) -> Bool
dfs mark visited board (r, c)
    | mark == Nought && r == length board - 1 = True
    | mark == Cross && c == length (head board) - 1 = True
    | otherwise = 
        let neighbors = getNeighbors (r, c) board
            unvisited = filter (`Set.notMember` visited) neighbors
            newVisited = Set.insert (r, c) visited
        in any (dfs mark newVisited board) unvisited

getNeighbors :: (Int, Int) -> [String] -> [(Int, Int)]
getNeighbors (r, c) board = 
    [(nr, nc) | (nr, nc) <- [(r-1, c), (r+1, c), (r, c-1), (r, c+1), (r-1, c+1), (r+1, c-1)],
                 nr >= 0, nr < length board, nc >= 0, nc < length (head board),
                 board !! nr !! nc == if mark == Nought then 'O' else 'X']
