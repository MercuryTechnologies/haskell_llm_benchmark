module School (School, add, empty, grade, sorted) where

import qualified Data.Map as Map
import Data.List (sort)

newtype School = School (Map.Map Int [String])

empty :: School
empty = School Map.empty

add :: Int -> String -> School -> School
add gradeNum student (School m) = School (Map.insertWith (++) gradeNum [student] m)

grade :: Int -> School -> [String]
grade gradeNum (School m) =
  maybe [] sort (Map.lookup gradeNum m)

sorted :: School -> [(Int, [String])]
sorted (School m) =
  map (\(gradeNum, names) -> (gradeNum, sort names)) $ Map.toAscList m
