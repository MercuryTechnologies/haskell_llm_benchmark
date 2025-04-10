module Connect (Mark(..), winner) where

import Data.Char (isAlpha)
import qualified Data.Set as S

data Mark = Cross | Nought deriving (Eq, Show)

type Coord = (Int, Int)
type Board = S.Set Coord

winner :: [String] -> Maybe Mark
winner board = 
  let size = length board
      crossCoords = S.fromList [(x, y) | y <- [0..size-1], x <- [0..size-y-1] | x < size, let c = (board !! y) !! (x + y), c == 'X']
      noughtCoords = S.fromList [(x, y) | y <- [0..size-1], x <- [0..size-y-1] | x < size, let c = (board !! y) !! (x + y), c == 'O']
      hasCrossPath = hasPath Cross size crossCoords
      hasNoughtPath = hasPath Nought size noughtCoords
  in if hasCrossPath then Just Cross
     else if hasNoughtPath then Just Nought
     else Nothing

hasPath :: Mark -> Int -> Board -> Bool
hasPath mark size board = 
  let (startCoords, endCol) = case mark of
                                Cross -> ([(x, 0) | x <- [0..size-1]], \c -> snd c == size - 1)
                                Nought -> ([(0, y) | y <- [0..size-1]], \c -> fst c == size - 1)
      starts = filter (`S.member` board) startCoords
  in any (\start -> dfs start board endCol S.empty) starts

dfs :: Coord -> Board -> (Coord -> Bool) -> S.Set Coord -> Bool
dfs coord board endCol visited =
  if coord `S.member` visited then False
  else if endCol coord then True
  else let visited' = S.insert coord visited
           neighbors = filter (`S.member` board) (neighbors' coord)
       in any (\n -> dfs n board endCol visited') neighbors

neighbors' :: Coord -> [Coord]
neighbors' (x, y) = [(x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y)]
