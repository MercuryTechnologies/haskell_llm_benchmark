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

data Garden = Garden [(String, [Plant])]
  deriving (Show)

garden :: [String] -> String -> Garden
garden students diagram =
    let sortedStudents = sort students
        ls = lines diagram
        row1 = case ls of
                 (r1:r2:_) -> r1
                 _         -> ""
        row2 = case ls of
                 (r1:r2:_) -> r2
                 _         -> ""
        pairs1 = chunksOf 2 row1
        pairs2 = chunksOf 2 row2
        entries = zipWith (\s (p1, p2) -> (s, parsePlants (p1 ++ p2))) 
                          sortedStudents 
                          (zip pairs1 pairs2)
    in Garden entries

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden entries) =
    case lookup student entries of
      Just ps -> ps
      Nothing -> []

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

parsePlants :: String -> [Plant]
parsePlants = map toPlant
  where
    toPlant 'C' = Clover
    toPlant 'G' = Grass
    toPlant 'R' = Radishes
    toPlant 'V' = Violets
    toPlant x   = error ("Unknown plant: " ++ [x])
