module Connect (Mark(..), winner) where

import Data.Maybe (mapMaybe)
import Data.List (transpose)
import qualified Data.Set as Set

data Mark = Cross | Nought deriving (Eq, Show)

type Board = [[Cell]]
data Cell = Empty | X | O deriving (Eq, Show)

-- Parse the board from [String] to [[Cell]]
parseBoard :: [String] -> Board
parseBoard = map (map charToCell . dropWhile (== ' '))
  where
    charToCell '.' = Empty
    charToCell 'X' = X
    charToCell 'O' = O
    charToCell _   = Empty

-- Get the size of the board
boardSize :: Board -> (Int, Int)
boardSize b = (length b, if null b then 0 else length (head b))

-- Get all neighbors of a cell in hex grid
neighbors :: (Int, Int) -> Board -> [(Int, Int)]
neighbors (r, c) board =
  let (h, w) = boardSize board
      deltas = [(-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0)]
      inside (x, y) = x >= 0 && y >= 0 && x < h && y < w
  in filter inside [(r + dr, c + dc) | (dr, dc) <- deltas]

-- Find if a player has a path from their side to the opposite
hasWon :: Cell -> Board -> Bool
hasWon player board =
  let (h, w) = boardSize board
      -- For O: top to bottom (rows)
      -- For X: left to right (cols)
      starts = case player of
        O -> [(0, c) | c <- [0..w-1], board !! 0 !! c == O]
        X -> [(r, 0) | r <- [0..h-1], board !! r !! 0 == X]
        _ -> []
      goal (r, c) = case player of
        O -> r == h - 1
        X -> c == w - 1
        _ -> False
      go visited [] = False
      go visited (p:ps)
        | goal p = True
        | p `Set.member` visited = go visited ps
        | otherwise =
            let ns = [n | n <- neighbors p board, boardCell n == player]
                boardCell (r, c) = board !! r !! c
            in go (Set.insert p visited) (ns ++ ps)
  in go Set.empty starts

winner :: [String] -> Maybe Mark
winner boardStrs =
  let board = parseBoard boardStrs
  in case (hasWon X board, hasWon O board) of
       (True, _)    -> Just Cross
       (_, True)    -> Just Nought
       _            -> Nothing
