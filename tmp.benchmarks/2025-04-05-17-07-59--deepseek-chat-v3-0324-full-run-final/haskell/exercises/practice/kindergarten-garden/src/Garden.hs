module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort)
import Data.Map (Map, fromList, (!))
import Data.Maybe (fromMaybe)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden (Map String [Plant]) deriving (Eq, Show)

defaultStudents :: [String]
defaultStudents = ["Alice", "Bob", "Charlie", "David", 
                  "Eve", "Fred", "Ginny", "Harriet", 
                  "Ileana", "Joseph", "Kincaid", "Larry"]

charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant _   = error "Invalid plant character"

garden :: [String] -> String -> Garden
garden students plants = Garden $ fromList $ zip sortedStudents plantAssignments
  where
    sortedStudents = sort students
    [row1, row2] = lines plants
    cupsPerStudent = 2
    plantChars = zipWith (++) (chunksOf cupsPerStudent row1) (chunksOf cupsPerStudent row2)
    plantAssignments = map (map charToPlant . concat) plantChars

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden m) = fromMaybe [] (lookup student m)

-- Helper function to split a list into chunks of size n
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
