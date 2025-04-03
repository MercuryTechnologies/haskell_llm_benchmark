module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (elemIndex)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden [[Plant]] deriving (Show)

garden :: [String] -> String -> Garden
garden students plants = Garden (map (map plantFromChar) (rows plants))
  where
    rows xs = [take 4 (drop (i * 4) xs) | i <- [0..(length xs `div` 4 - 1)]]
    plantFromChar 'G' = Grass
    plantFromChar 'C' = Clover
    plantFromChar 'R' = Radishes
    plantFromChar 'V' = Violets
    plantFromChar _   = error "Invalid plant character"

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden plantRows) = concatMap (take 2 . drop (studentIndex * 2)) (take 2 plantRows)
  where
    students = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]
    studentIndex = case elemIndex student students of
      Just index -> index
      Nothing    -> error "Student not found"
