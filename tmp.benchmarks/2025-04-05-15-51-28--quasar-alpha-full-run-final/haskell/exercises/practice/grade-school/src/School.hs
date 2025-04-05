module School (School, add, empty, grade, sorted) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (sort)

newtype School = School (Map Int [String])
  deriving (Eq, Show)

empty :: School
empty = School Map.empty

add :: Int -> String -> School -> School
add gradeNum student (School m) =
  let updated = Map.insertWith (\new old -> sort (student : old)) gradeNum [student] m
  in School updated

grade :: Int -> School -> [String]
grade gradeNum (School m) =
  case Map.lookup gradeNum m of
    Just students -> sort students
    Nothing -> []

sorted :: School -> [(Int, [String])]
sorted (School m) =
  [ (g, sort students) | (g, students) <- Map.toAscList m ]
