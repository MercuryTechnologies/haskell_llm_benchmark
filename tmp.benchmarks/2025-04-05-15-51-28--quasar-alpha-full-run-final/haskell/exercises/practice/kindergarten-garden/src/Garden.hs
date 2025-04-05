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

data Garden = Garden [(String, [Plant])]
    deriving (Eq, Show)

defaultStudents :: [String]
defaultStudents = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny",
                   "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]

plantFromChar :: Char -> Plant
plantFromChar 'G' = Grass
plantFromChar 'C' = Clover
plantFromChar 'R' = Radishes
plantFromChar 'V' = Violets
plantFromChar _   = error "Unknown plant character"

garden :: [String] -> String -> Garden
garden students plantsStr =
    let studentsSorted = sort students
        (row1:row2:_) = lines plantsStr
        pairs1 = chunksOf 2 row1
        pairs2 = chunksOf 2 row2
        studentPlants = zipWith3 (\name p1 p2 -> (name, map plantFromChar (p1 ++ p2)))
                                 studentsSorted pairs1 pairs2
    in Garden studentPlants

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden studentPlants) =
    case lookup student studentPlants of
        Just ps -> ps
        Nothing -> []

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t
