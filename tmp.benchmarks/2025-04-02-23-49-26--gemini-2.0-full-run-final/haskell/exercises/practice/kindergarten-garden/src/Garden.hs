module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort)
import Data.Map (Map, fromList, lookup)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type GardenMap = Map String [Plant]

garden :: [String] -> String -> GardenMap
garden students plants =
  let
    sortedStudents = sort students
    rows = lines plants
    row1 = head rows
    row2 = last rows
    plantPairs = zip (chunksOf 2 row1) (chunksOf 2 row2)
    studentPlants = zip sortedStudents plantPairs
  in
    fromList $ map (\(student, (p1, p2)) -> (student, plantsFromPairs p1 p2)) studentPlants

lookupPlants :: String -> GardenMap -> [Plant]
lookupPlants student garden =
  case Data.Map.lookup student garden of
    Just plants -> plants
    Nothing -> []

chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n xs = let (chunk, rest) = splitAt n xs in chunk : chunksOf n rest

plantFromChar :: Char -> Plant
plantFromChar 'G' = Grass
plantFromChar 'C' = Clover
plantFromChar 'R' = Radishes
plantFromChar 'V' = Violets
plantFromChar _   = error "Invalid plant character"

plantsFromPairs :: String -> String -> [Plant]
plantsFromPairs p1 p2 = map plantFromChar (p1 ++ p2)
