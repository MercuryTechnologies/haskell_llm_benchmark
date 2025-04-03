module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Student = String
type Garden = Map.Map Student [Plant]

studentsList :: [Student]
studentsList = sort ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]

charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant _   = error "Invalid plant character"

garden :: [Student] -> String -> Garden
garden students plants = Map.fromList $ zip sortedStudents studentPlants
  where
    sortedStudents = sort students
    rows = lines plants
    studentPlants = map (map charToPlant) $ chunksOf 2 (concatMap (chunksOf 2) rows)

lookupPlants :: Student -> Garden -> [Plant]
lookupPlants student garden = fromMaybe [] (Map.lookup student garden)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
