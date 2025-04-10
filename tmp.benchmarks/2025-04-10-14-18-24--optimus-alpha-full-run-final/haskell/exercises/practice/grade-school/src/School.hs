module School (School, add, empty, grade, sorted) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (sort)

-- School is a map from grade number to a list of student names
newtype School = School (Map Int [String])
  deriving (Eq, Show)

-- Create an empty school
empty :: School
empty = School Map.empty

-- Add a student to a grade in the school
add :: Int -> String -> School -> School
add gradeNum student (School m) =
  School $ Map.alter (Just . insertStudent student) gradeNum m
  where
    insertStudent s Nothing = [s]
    insertStudent s (Just xs) = s : xs

-- Get a list of students in a grade, sorted alphabetically
grade :: Int -> School -> [String]
grade gradeNum (School m) =
  maybe [] sort (Map.lookup gradeNum m)

-- Get a sorted list of all grades and their students (students sorted alphabetically)
sorted :: School -> [(Int, [String])]
sorted (School m) =
  [ (g, sort xs) | (g, xs) <- Map.toAscList m ]
