module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as S

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)  -- (x, y)

--------------------------------------------------------------------------------
-- Public API

-- | Given a board (list of lines), return all territories discovered,
--   along with the single color that owns each territory (if any).
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board =
    let coords = allCoords board
        visited = S.empty
    in territoriesImpl board coords visited

-- | Given a board and a coordinate, return the territory that includes
--   that coordinate (if the coordinate is empty), along with the single
--   color that owns that territory (if any).
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board (x, y)
    | not (isOnBoard board (x, y))     = Nothing
    | not (isEmptyCoord board (x, y))  = Nothing
    | otherwise =
        let (reg, adjacentColors) = flood board (x, y)
            colorOwner = determineOwner adjacentColors
        in Just (reg, colorOwner)

--------------------------------------------------------------------------------
-- Implementation

-- | Return a list of all valid coordinates on the board.
allCoords :: [String] -> [Coord]
allCoords board =
    [ (x, y)
    | y <- [1 .. length board]
    , x <- [1 .. length (board !! (y - 1))]
    ]

-- | Recursively explore territories from each unvisited empty coordinate.
territoriesImpl :: [String]
                -> [Coord]
                -> Set Coord
                -> [(Set Coord, Maybe Color)]
territoriesImpl _     []     _       = []
territoriesImpl board (p:ps) visited
    | S.member p visited = territoriesImpl board ps visited
    | isEmptyCoord board p =
        let (reg, adjColors) = flood board p
            newVisited       = S.union visited reg
            owner            = determineOwner adjColors
        in (reg, owner) : territoriesImpl board ps newVisited
    | otherwise = territoriesImpl board ps visited

-- | Flood fill (DFS or BFS) starting from a given empty coordinate,
--   gathering all coordinates in that territory, as well as all
--   distinct colors that border it.
flood :: [String]
      -> Coord
      -> (Set Coord, Set Color)
flood board start =
    let go toVisit region adjColors
            | null toVisit = (region, adjColors)
            | otherwise =
                let current          = head toVisit
                    rest             = tail toVisit
                    ns               = neighbors board current
                    (empties, stones) = partition (isEmptyCoord board) ns
                    newRegion        = foldr S.insert region empties
                    newAdjCols       = S.union adjColors (S.fromList (mapMaybe (parseColor . charAt board) stones))
                    notVisited       = filter (`S.notMember` region) empties
                in go (rest ++ notVisited) newRegion newAdjCols
    in go [start] (S.singleton start) S.empty

-- | Determine the territory owner from the set of adjacent colors.
--   If exactly one color is present, that's the owner.
--   Otherwise, the territory belongs to no one.
determineOwner :: Set Color -> Maybe Color
determineOwner adjColors
    | S.size adjColors == 1 = Just (head (S.elems adjColors))
    | otherwise             = Nothing

--------------------------------------------------------------------------------
-- Helpers

-- | Check if a coordinate is on the board.
isOnBoard :: [String] -> Coord -> Bool
isOnBoard board (x, y) =
    y >= 1 && y <= length board &&
    x >= 1 && x <= length (board !! (y - 1))

-- | Return the character at a board coordinate ((x, y) is 1-based).
charAt :: [String] -> Coord -> Char
charAt board (x, y) = (board !! (y - 1)) !! (x - 1)

-- | Determine if the given coordinate is empty.
isEmptyCoord :: [String] -> Coord -> Bool
isEmptyCoord board (x, y) = charAt board (x, y) == ' '

-- | Parse a board character into a possible Color.
parseColor :: Char -> Maybe Color
parseColor 'B' = Just Black
parseColor 'W' = Just White
parseColor  _  = Nothing

-- | Return the list of orthogonal neighbors of a coordinate
--   that are within the board.
neighbors :: [String] -> Coord -> [Coord]
neighbors board (x, y) =
    filter (isOnBoard board)
    [ (x - 1, y)
    , (x + 1, y)
    , (x, y - 1)
    , (x, y + 1)
    ]

-- | Partition function for convenience,
--   grouping coordinates into empties vs stones.
partition :: (Coord -> Bool) -> [Coord] -> ([Coord], [Coord])
partition _ [] = ([], [])
partition f (c:cs) =
    let (ts, fs) = partition f cs
    in if f c
       then (c:ts, fs)
       else (ts, c:fs)

-- | A maybe-like helper for convenience.
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) =
    case f x of
        Just y  -> y : mapMaybe f xs
        Nothing -> mapMaybe f xs
