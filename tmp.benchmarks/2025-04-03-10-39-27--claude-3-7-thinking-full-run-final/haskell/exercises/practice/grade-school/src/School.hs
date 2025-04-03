module School (School, add, empty, grade, sorted) where

import Data.List (sort)

newtype School = School [(Int, [String])]

add :: Int -> String -> School -> School
add gradeNum student (School entries) =
  let 
    existingStudents = gradeEntries gradeNum entries
    newEntry = (gradeNum, existingStudents ++ [student])
  in
    School (newEntry : filter (\(g, _) -> g /= gradeNum) entries)

empty :: School
empty = School []

grade :: Int -> School -> [String]
grade gradeNum (School entries) = gradeEntries gradeNum entries

gradeEntries :: Int -> [(Int, [String])] -> [String]
gradeEntries gradeNum entries = 
  case lookup gradeNum entries of
    Nothing -> []
    Just students -> students

sorted :: School -> [(Int, [String])]
sorted (School entries) = 
  [(g, sort students) | (g, students) <- sort entries]
