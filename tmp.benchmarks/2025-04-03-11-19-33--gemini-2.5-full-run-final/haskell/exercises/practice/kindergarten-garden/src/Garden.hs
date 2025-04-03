module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Data.List (sort, lines)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

-- Garden stores a mapping from student name to their list of plants.
newtype Garden = Garden (Map String [Plant]) deriving (Eq, Show)

-- Helper function to convert a character to a Plant.
charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant c   = error $ "Invalid plant character: " ++ [c]

-- Creates a Garden map from a list of students and a diagram string.
garden :: [String] -> String -> Garden
garden students diagram = Garden $ Map.fromList (zip sortedStudents plantLists)
  where
    sortedStudents = sort students
    [row1, row2] = lines diagram -- Assumes diagram has exactly two lines

    -- Generates the list of plants for each student based on their index.
    plantLists :: [[Plant]]
    plantLists = map studentPlants [0 .. length sortedStudents - 1]

    -- Gets the four plants for a student at a given index.
    studentPlants :: Int -> [Plant]
    studentPlants i = map charToPlant [ plantAt row1 (2*i)
                                      , plantAt row1 (2*i + 1)
                                      , plantAt row2 (2*i)
                                      , plantAt row2 (2*i + 1)
                                      ]

    -- Safe way to get character at index, handles potential out-of-bounds.
    plantAt :: String -> Int -> Char
    plantAt row idx
      | idx < length row = row !! idx
      | otherwise        = error "Diagram row is too short for the number of students."


-- Looks up the plants for a given student in the garden.
lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden gardenMap) =
    -- Using (!) assumes the student will always be in the map
    -- if the garden was constructed with them.
    -- If a student not in the original list is looked up, it will error.
    gardenMap ! student
