module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden [(String, [Plant])] deriving (Eq, Show)

plantFromChar :: Char -> Plant
plantFromChar 'C' = Clover
plantFromChar 'G' = Grass
plantFromChar 'R' = Radishes
plantFromChar 'V' = Violets
plantFromChar _   = error "Invalid plant character."

garden :: [String] -> String -> Garden
garden students plantsStr =
    let sortedStudents = sort students
        rows           = lines plantsStr
        row1           = head rows
        row2           = rows !! 1
        pairs = zip sortedStudents $ map (\i ->
            [ plantFromChar (row1 !! (2*i))
            , plantFromChar (row1 !! (2*i + 1))
            , plantFromChar (row2 !! (2*i))
            , plantFromChar (row2 !! (2*i + 1))
            ])
            [0 .. length sortedStudents - 1]
    in Garden pairs

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden pairs) =
    case lookup student pairs of
      Just ps -> ps
      Nothing -> []
