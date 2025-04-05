module Connect (Mark(..), winner) where

import Data.List (transpose)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner board
  | hasPath Nought noughtPositions noughtGoal = Just Nought
  | hasPath Cross crossPositions crossGoal = Just Cross
  | otherwise = Nothing
  where
    height = length board
    width = if null board then 0 else length (head board)

    -- Positions of each player's marks
    noughtPositions = [ (r, c) | (r, row) <- zip [0..] board
                               , (c, cell) <- zip [0..] row
                               , cell == 'O' ]
    crossPositions  = [ (r, c) | (r, row) <- zip [0..] board
                               , (c, cell) <- zip [0..] row
                               , cell == 'X' ]

    -- Starting positions and goal test for each player
    noughtGoal pos = fst pos == height - 1
    crossGoal pos  = snd pos == width - 1

    -- Check if a player has a path from their starting edge to their goal edge
    hasPath mark positions isGoal =
      let startPositions = filter (isStart mark) positions
      in any (\p -> search mark p Set.empty isGoal) startPositions

    -- Starting edge for each player
    isStart Nought (r, _) = r == 0
    isStart Cross  (_, c) = c == 0

    -- DFS search from a position
    search mark pos visited isGoal
      | isGoal pos = True
      | otherwise =
          let visited' = Set.insert pos visited
              neighbors = adjacent pos
              nextPositions = [ p | p <- neighbors
                                  , not (Set.member p visited')
                                  , inBounds p
                                  , cellAt p == markChar mark ]
          in any (\p -> search mark p visited' isGoal) nextPositions

    -- Character representing each player
    markChar Nought = 'O'
    markChar Cross  = 'X'

    -- Check if a position is within the board
    inBounds (r, c) = r >= 0 && r < height && c >= 0 && c < width

    -- Get the character at a position
    cellAt (r, c) = (board !! r) !! c

    -- Adjacent hex neighbors
    adjacent (r, c) =
      [ (r-1, c)     -- up
      , (r-1, c+1)   -- up-right
      , (r, c-1)     -- left
      , (r, c+1)     -- right
      , (r+1, c-1)   -- down-left
      , (r+1, c)     -- down
      ]
