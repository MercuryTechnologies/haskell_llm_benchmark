module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    , defaultGarden
    , defaultStudents
    ) where

import Data.List (sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden { plants :: [[Plant]], students :: [(String, Int)] }

defaultStudents :: [String]
defaultStudents = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]

defaultGarden :: String -> Garden
defaultGarden plantsStr = garden defaultStudents plantsStr

garden :: [String] -> String -> Garden
garden studentsStr plantsStr = Garden { plants = parsePlants plantsStr, students = assignStudents (sort studentsStr) }

parsePlants :: String -> [[Plant]]
parsePlants plantsStr = map (map parsePlant) (lines plantsStr)
  where
    parsePlant 'C' = Clover
    parsePlant 'G' = Grass
    parsePlant 'R' = Radishes
    parsePlant 'V' = Violets
    parsePlant c   = error $ "Invalid plant: " ++ [c]

assignStudents :: [String] -> [(String, Int)]
assignStudents studentsStr = zip studentsStr [0..]

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student garden = let
  studentIndex = case lookup student (students garden) of
    Just index -> index
    Nothing    -> error $ "Unknown student: " ++ student
  plantsList = plants garden
  in [plantsList !! 0 !! (studentIndex * 2), plantsList !! 0 !! (studentIndex * 2 + 1), plantsList !! 1 !! (studentIndex * 2), plantsList !! 1 !! (studentIndex * 2 + 1)]
