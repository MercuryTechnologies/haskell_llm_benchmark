module School (School, add, empty, grade, sorted) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

-- Use a Map where keys are grade numbers and values are sorted lists of student names.
type School = Map.Map Int [String]

-- | Creates an empty school roster.
empty :: School
empty = Map.empty

-- | Adds a student to a specific grade in the school roster.
-- If the grade already exists, the student is added to the list,
-- and the list is kept sorted alphabetically.
-- If the grade doesn't exist, it's created with the student.
add :: Int -> String -> School -> School
add gradeNum student school = Map.insertWith merge gradeNum [student] school
  where
    -- Function to merge a new student into an existing list and keep it sorted.
    merge :: [String] -> [String] -> [String]
    merge newStudentList existingStudents = List.sort (newStudentList ++ existingStudents)

-- | Returns the list of students in a specific grade.
-- Returns an empty list if the grade doesn't exist or has no students.
-- The list of students is guaranteed to be sorted alphabetically.
grade :: Int -> School -> [String]
grade gradeNum school = fromMaybe [] (Map.lookup gradeNum school)

-- | Returns the entire school roster sorted by grade.
-- Within each grade, students are sorted alphabetically.
sorted :: School -> [(Int, [String])]
sorted = Map.toAscList -- Map.toAscList returns the key-value pairs sorted by key (grade).
                       -- The student lists (values) are already sorted by the `add` function.

