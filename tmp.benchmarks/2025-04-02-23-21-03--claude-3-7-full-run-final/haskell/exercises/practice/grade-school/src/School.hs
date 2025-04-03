module School (School, add, empty, grade, sorted) where

import Data.List (sort)
import qualified Data.Map as Map

-- School is represented as a Map from grade number to a list of student names
newtype School = School (Map.Map Int [String])
  deriving (Eq, Show)

-- Add a student to a grade in the school
add :: Int -> String -> School -> School
add gradeNum student (School schoolMap) = 
  School $ Map.insertWith (++) gradeNum [student] schoolMap

-- Create an empty school with no students
empty :: School
empty = School Map.empty

-- Get all students in a specific grade
grade :: Int -> School -> [String]
grade gradeNum (School schoolMap) = 
  sort $ Map.findWithDefault [] gradeNum schoolMap

-- Get a sorted list of all students in all grades
sorted :: School -> [(Int, [String])]
sorted (School schoolMap) = 
  map (\(g, students) -> (g, sort students)) $ 
  Map.toAscList schoolMap
