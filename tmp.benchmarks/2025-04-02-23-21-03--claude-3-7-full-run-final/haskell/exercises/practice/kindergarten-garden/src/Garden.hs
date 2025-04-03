module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.Maybe (fromMaybe)
import Data.List (find)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden [[Plant]]
             deriving (Eq, Show)

-- | Convert a character to a Plant
charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant _   = error "Unknown plant"

-- | Default list of students
defaultStudents :: [String]
defaultStudents = ["Alice", "Bob", "Charlie", "David", 
                  "Eve", "Fred", "Ginny", "Harriet", 
                  "Ileana", "Joseph", "Kincaid", "Larry"]

-- | Create a garden from a diagram and a list of students
garden :: [String] -> String -> Garden
garden students plants = Garden plantRows
  where
    rows = lines plants
    plantRows = map (map charToPlant) rows

-- | Look up the plants for a specific student
lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden rows) = 
    let studentIndex = fromMaybe 0 (findIndex student (defaultStudents))
        startPos = studentIndex * 2
        endPos = startPos + 1
        plantsForStudent = concatMap (getPlantsInRange startPos endPos) rows
    in plantsForStudent
  where
    findIndex s ss = 
      case find (== s) ss of
        Just val -> Just (length $ takeWhile (/= val) ss)
        Nothing -> Nothing
    
    getPlantsInRange start end row =
      if length row > end
        then take (end - start + 1) (drop start row)
        else []
