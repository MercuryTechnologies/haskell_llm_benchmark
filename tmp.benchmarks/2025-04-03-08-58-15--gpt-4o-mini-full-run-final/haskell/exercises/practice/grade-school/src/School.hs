module School (School, add, empty, grade, sorted) where

import Data.List (sort)
import qualified Data.Map as Map

data School = School (Map.Map Int [String])

add :: Int -> String -> School -> School
add gradeNum student (School roster) =
    School (Map.insertWith (++) gradeNum [student] roster)

empty :: School
empty = School Map.empty

grade :: Int -> School -> [String]
grade gradeNum (School roster) =
    Map.findWithDefault [] gradeNum roster

sorted :: School -> [(Int, [String])]
sorted (School roster) =
    [(gradeNum, sort students) | (gradeNum, students) <- Map.toList roster]
