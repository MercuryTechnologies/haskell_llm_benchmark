module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
    -- Check for invalid side lengths (non-positive)
    | a <= 0 || b <= 0 || c <= 0 = Illegal
    -- Check triangle inequality
    | a + b < c || b + c < a || a + c < b = Illegal
    -- Check for equilateral (all sides equal)
    | a == b && b == c = Equilateral
    -- Check for isosceles (at least two sides equal)
    | a == b || b == c || a == c = Isosceles
    -- Otherwise, it's scalene (all sides different)
    | otherwise = Scalene
