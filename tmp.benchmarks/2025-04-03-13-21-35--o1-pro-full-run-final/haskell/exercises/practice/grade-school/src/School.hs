module School (School, add, empty, grade, sorted) where

import Data.List (sort, sortBy)
import Data.Ord (comparing)

-- We'll track the school's roster as a list of (grade, [student]) pairs.
-- Each student's list is kept sorted so that a direct 'grade' query returns
-- names in sorted order. For a full sorted overview, we'll also sort the
-- outer list when returning 'sorted'.

data School = School [(Int, [String])]
  deriving (Show)

-- Create an empty school.
empty :: School
empty = School []

-- Add a student to the specific grade. If the grade doesn't exist yet,
-- create it; otherwise add the student to that grade.
add :: Int -> String -> School -> School
add gradeNum student (School s) =
  let s' = addHelper s
   in School s'
  where
    addHelper [] = [(gradeNum, [student])]
    addHelper ((g, names):rest)
      | g == gradeNum = (g, sort (student : names)) : rest
      | otherwise = (g, names) : addHelper rest

-- Return all the students in a specific grade (sorted by name).
grade :: Int -> School -> [String]
grade g (School s) =
  case lookup g s of
    Just names -> names
    Nothing    -> []

-- Return a list of (grade, [students]) sorted by grade.
-- Each grade's student list is guaranteed sorted by the line above.
sorted :: School -> [(Int, [String])]
sorted (School s) =
  let sortedGrades = sortBy (comparing fst) s
  in map (\(g, names) -> (g, sort names)) sortedGrades
