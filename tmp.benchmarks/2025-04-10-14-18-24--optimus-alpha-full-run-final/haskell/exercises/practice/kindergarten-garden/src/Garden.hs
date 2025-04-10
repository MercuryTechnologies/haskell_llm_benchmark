module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden [String] [[Plant]]
    deriving (Eq, Show)

-- Map a character to a Plant
charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant c   = error $ "Unknown plant code: " ++ [c]

-- Build the garden from the student list and diagram string
garden :: [String] -> String -> Garden
garden students plants =
    let studentList = sort students
        rows = lines plants
        -- Each row must have 2 cups per student
        plantRows = map (map charToPlant) rows
    in Garden studentList plantRows

-- Lookup the plants for a given student
lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden students plantRows) =
    let idx = fromJust $ elemIndex student students
        -- Each student gets 2 cups per row, so their cups are at positions 2*idx and 2*idx+1
        getPlants row = take 2 . drop (2 * idx) $ row
    in concatMap getPlants plantRows
