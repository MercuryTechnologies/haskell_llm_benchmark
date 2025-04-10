module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.Maybe (listToMaybe)

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

type Grid = [String]
type Direction = (Int, Int)

-- All 8 possible directions: (dx, dy)
directions :: [Direction]
directions = [ (1,0)   -- right
             , (-1,0)  -- left
             , (0,1)   -- down
             , (0,-1)  -- up
             , (1,1)   -- down-right
             , (-1,1)  -- down-left
             , (1,-1)  -- up-right
             , (-1,-1) -- up-left
             ]

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\w -> (w, findWord grid w)) wordList

findWord :: Grid -> String -> Maybe WordPos
findWord grid word =
    listToMaybe [ WordPos (CharPos (c+1) (r+1)) (CharPos (c+1 + dx * (len-1)) (r+1 + dy * (len-1)))
                | r <- [0..numRows-1]
                , c <- [0..numCols-1]
                , (dx, dy) <- directions
                , fitsInGrid c r dx dy len numCols numRows
                , matchAt grid word c r dx dy
                ]
  where
    len = length word
    numRows = length grid
    numCols = if null grid then 0 else length (head grid)

fitsInGrid :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
fitsInGrid c r dx dy len numCols numRows =
    let endC = c + dx * (len-1)
        endR = r + dy * (len-1)
    in endC >= 0 && endC < numCols && endR >= 0 && endR < numRows

matchAt :: Grid -> String -> Int -> Int -> Int -> Int -> Bool
matchAt grid word c r dx dy =
    and [ safeIndex grid (r + i*dy) (c + i*dx) == Just (word !! i)
        | i <- [0..length word - 1]
        ]

safeIndex :: [String] -> Int -> Int -> Maybe Char
safeIndex grid r c
    | r < 0 || r >= length grid = Nothing
    | c < 0 || c >= length (grid !! r) = Nothing
    | otherwise = Just ((grid !! r) !! c)
