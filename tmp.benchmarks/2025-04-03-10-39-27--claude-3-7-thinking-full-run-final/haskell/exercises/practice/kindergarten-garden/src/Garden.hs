module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (elemIndex, sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

-- A Garden consists of sorted student names and rows of plants
data Garden = Garden [String] [[Plant]]

-- Convert a character to a Plant
charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant _   = error "Unknown plant"

-- Create a Garden from a list of students and a plant diagram string
garden :: [String] -> String -> Garden
garden students plantStr = 
    let rows = lines plantStr
        plantRows = map (map charToPlant) rows
    in Garden (sort students) plantRows

-- Look up a student's plants in a Garden
lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden students plantRows) = 
    case elemIndex student students of
        Just idx -> 
            let startIdx = idx * 2
                plantsFromRow row = take 2 (drop startIdx row)
            in concatMap plantsFromRow plantRows
        Nothing -> []
