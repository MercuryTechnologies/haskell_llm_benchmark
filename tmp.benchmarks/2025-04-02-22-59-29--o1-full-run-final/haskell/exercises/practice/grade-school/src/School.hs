module School (School, add, empty, grade, sorted) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List (sort)

-- We'll store the roster in a Map of grade to a list of student names.
-- 'empty' is simply an empty map, 
-- 'add' inserts a student into the list of names for that grade,
-- 'grade' returns the alphabetically sorted names for the given grade,
-- 'sorted' returns a list of (grade, [sorted names]) in ascending order of grade.

data School = School (Map Int [String])
  deriving (Show, Eq)

empty :: School
empty = School M.empty

add :: Int -> String -> School -> School
add gradeNum student (School roster) =
  case M.lookup gradeNum roster of
    Nothing -> School (M.insert gradeNum [student] roster)
    Just students -> School (M.insert gradeNum (student : students) roster)

grade :: Int -> School -> [String]
grade gradeNum (School roster) =
  sort (M.findWithDefault [] gradeNum roster)

sorted :: School -> [(Int, [String])]
sorted (School roster) =
  let sortedGrades = sort (M.keys roster)
  in [(g, sort (M.findWithDefault [] g roster)) | g <- sortedGrades]
