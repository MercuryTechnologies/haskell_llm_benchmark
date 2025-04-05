module School (School, add, empty, grade, sorted) where

import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map

newtype School = School (Map Int [String]) deriving (Eq, Show)

add :: Int -> String -> School -> School
add gradeNum student (School grades) = 
    School $ Map.insertWith (++) gradeNum [student] grades

empty :: School
empty = School Map.empty

grade :: Int -> School -> [String]
grade gradeNum (School grades) = 
    sort $ Map.findWithDefault [] gradeNum grades

sorted :: School -> [(Int, [String])]
sorted (School grades) = 
    map (\(g, ss) -> (g, sort ss)) $ Map.toAscList grades
