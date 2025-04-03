module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as S

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

-- | Determine all territories on the board along with ownership.
--   Each element of the result is (coords, owner) for one connected territory of empty spots.
--   The owner is 'Just Black', 'Just White', or Nothing if shared/unowned.
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board =
    let allCoords = [(r, c) | r <- [1 .. boardWidth board]
                            , c <- [1 .. boardHeight board]]
        emptyCoords = filter (isEmpty board) allCoords
        go [] _ acc = acc
        go (x:xs) visited acc
          | x `S.member` visited = go xs visited acc
          | otherwise =
              let (empties, stones) = floodFill board x
                  newVisited        = visited `S.union` empties
                  territoryOwner    = owner stones
              in go xs newVisited ((empties, territoryOwner) : acc)
    in go emptyCoords S.empty []

-- | Determine the territory (coords, owner) that includes the given coordinate.
--   Return Nothing if the coordinate is not empty or if it doesn't exist on the board.
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord
  | not (inBounds board coord)     = Nothing
  | not (isEmpty board coord)      = Nothing
  | otherwise =
      let (empties, stones) = floodFill board coord
      in Just (empties, owner stones)

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

-- Returns the number of columns in each line.
boardHeight :: [String] -> Int
boardHeight []    = 0
boardHeight (r:_) = length r

-- Returns the number of lines of the board.
boardWidth :: [String] -> Int
boardWidth = length

-- Is the coordinate within the bounds of the board?
inBounds :: [String] -> Coord -> Bool
inBounds board (r, c) =
    r >= 1 && r <= boardWidth board &&
    c >= 1 && c <= boardHeight board

-- Return the character at the given coordinate (1-based).
charAt :: [String] -> Coord -> Char
charAt board (r, c) = (board !! (r - 1)) !! (c - 1)

-- Determine if a given coordinate is empty (not 'B' or 'W').
isEmpty :: [String] -> Coord -> Bool
isEmpty board coord = parseColor (charAt board coord) == Nothing

-- Convert a board character to a Maybe Color.
-- 'B' -> Just Black, 'W' -> Just White, otherwise Nothing.
parseColor :: Char -> Maybe Color
parseColor 'B' = Just Black
parseColor 'W' = Just White
parseColor _   = Nothing

-- Get the neighbors (up, down, left, right), 1-based.
neighbors :: Coord -> [Coord]
neighbors (r, c) = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

-- Flood fill from an empty square, collecting all connected empties and any stones adjacent to them.
-- Returns the set of empty coordinates reached and the set of Colors of adjacent stones.
floodFill :: [String] -> Coord -> (Set Coord, Set Color)
floodFill board start = go S.empty S.empty [start]
  where
    go empties stones [] = (empties, stones)
    go empties stones (p:ps)
      | not (inBounds board p)         = go empties stones ps
      | p `S.member` empties           = go empties stones ps
      | parseColor (charAt board p) == Nothing =
          let empties' = S.insert p empties
          in go empties' stones (neighbors p ++ ps)
      | otherwise =
          let colorMaybe = parseColor (charAt board p)
              stones'    = case colorMaybe of
                             Just col -> S.insert col stones
                             Nothing  -> stones
          in go empties stones' ps

-- Given a set of colors, figure out if the territory is owned by exactly one color.
-- If the set is empty or has more than one distinct color, ownership is Nothing.
owner :: Set Color -> Maybe Color
owner stoneSet
  | S.size stoneSet == 1 = Just (head (S.elems stoneSet))
  | otherwise            = Nothing
