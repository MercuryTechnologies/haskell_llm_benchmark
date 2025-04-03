module School (School, add, empty, grade, sorted) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (sort)

data School = School { roster :: Map Int [String] } deriving (Show, Eq)

add :: Int -> String -> School -> School
add gradeNum student school =
  school { roster = Map.insertWith (++) gradeNum [student] (roster school) }

empty :: School
empty = School { roster = Map.empty }

grade :: Int -> School -> [String]
grade gradeNum school =
  case Map.lookup gradeNum (roster school) of
    Just students -> sort students
    Nothing       -> []

sorted :: School -> [(Int, [String])]
sorted school =
  map (\(gradeNum, students) -> (gradeNum, sort students)) $ Map.toList (roster school)
  -- sort the list of tuples by grade
  -- the grades are already Ints, so we can just compare them
  -- sortOn f x == sortBy (comparing f) x
  -- import Data.Ord (comparing)
  -- import Data.List (sortBy)
  -- sortBy (comparing fst) $ map (\(gradeNum, students) -> (gradeNum, sort students)) $ Map.toList (roster school)
