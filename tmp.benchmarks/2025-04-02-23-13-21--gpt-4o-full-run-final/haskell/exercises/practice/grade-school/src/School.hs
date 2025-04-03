module School (School, add, empty, grade, sorted) where

import qualified Data.Map as Map
import Data.List (sort)

-- Define the School data type as a Map from Int to a list of Strings
newtype School = School (Map.Map Int [String])

-- Add a student to a grade in the school
add :: Int -> String -> School -> School
add gradeNum student (School school) = 
    School $ Map.insertWith (++) gradeNum [student] school

-- Create an empty school
empty :: School
empty = School Map.empty

-- Get the list of students in a specific grade
grade :: Int -> School -> [String]
grade gradeNum (School school) = 
    Map.findWithDefault [] gradeNum school

-- Get a sorted list of all students in all grades
sorted :: School -> [(Int, [String])]
sorted (School school) = 
    map (\(g, s) -> (g, sort s)) $ Map.toAscList school
