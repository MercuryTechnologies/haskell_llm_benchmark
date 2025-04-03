module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort)
import qualified Data.Map as Map

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

newtype Garden = Garden (Map.Map String [Plant])
  deriving (Eq, Show)

toPlant :: Char -> Plant
toPlant 'C' = Clover
toPlant 'G' = Grass
toPlant 'R' = Radishes
toPlant 'V' = Violets
toPlant _   = error "Invalid plant character"

garden :: [String] -> String -> Garden
garden students diagram = Garden plantsMap
  where
    studentList = if null students then defaultStudents else sort students
    defaultStudents = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred",
                       "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]

    diagramLines = lines diagram
    row1 = if not (null diagramLines) then head diagramLines else ""
    row2 = if length diagramLines > 1 then diagramLines !! 1 else ""

    plantsMap = Map.fromList $ map (\(student, idx) ->
                      let cupIndices = [2 * idx, 2 * idx + 1]
                          cups1 = map (row1 !!) cupIndices
                          cups2 = map (row2 !!) cupIndices
                      in (student, map toPlant (cups1 ++ cups2))
                    ) (zip studentList [0..])

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden m) =
  case Map.lookup student m of
    Just plants -> plants
    Nothing -> []
