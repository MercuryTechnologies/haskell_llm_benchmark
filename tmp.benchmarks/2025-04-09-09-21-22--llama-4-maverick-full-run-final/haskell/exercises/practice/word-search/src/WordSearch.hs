module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.List (find)

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\word -> (word, findWord grid word)) wordList

findWord :: [String] -> String -> Maybe WordPos
findWord grid word = 
    let gridSize = length grid
        directions = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, -1), (1, -1), (-1, 1)]
    in case find (\(row, col, dir) -> isWordAt grid word row col dir) [(r, c, d) | r <- [0..gridSize-1], c <- [0..gridSize-1], d <- directions] of
        Just (row, col, dir) -> Just $ WordPos (CharPos col row) (CharPos (col + (length word - 1) * snd dir) (row + (length word - 1) * fst dir))
        Nothing -> Nothing

isWordAt :: [String] -> String -> Int -> Int -> (Int, Int) -> Bool
isWordAt grid word row col (dRow, dCol) =
    let gridSize = length grid
    in and [inGrid gridSize (row + i * dRow) (col + i * dCol) && (grid !! (row + i * dRow)) !! (col + i * dCol) == word !! i | i <- [0..length word - 1]]

inGrid :: Int -> Int -> Int -> Bool
inGrid size row col = row >= 0 && row < size && col >= 0 && col < size
