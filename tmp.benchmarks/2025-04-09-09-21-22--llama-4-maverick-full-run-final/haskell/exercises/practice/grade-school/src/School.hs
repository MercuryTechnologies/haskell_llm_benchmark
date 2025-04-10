module School (School, add, empty, grade, sorted) where

import qualified Data.Map as M
import Data.List (sort)

newtype School = School (M.Map Int [String])

empty :: School
empty = School M.empty

add :: Int -> String -> School -> School
add gradeNum student (School school) = 
    School $ M.insertWith (++) gradeNum [student] school

grade :: Int -> School -> [String]
grade gradeNum (School school) = 
    M.findWithDefault [] gradeNum school

sorted :: School -> [(Int, [String])]
sorted (School school) = 
    map (\(k, v) -> (k, sort v)) $ M.toAscList school
