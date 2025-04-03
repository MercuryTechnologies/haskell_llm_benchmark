module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Ord a, Num a) => a -> a -> a -> TriangleType
triangleType a b c
  | a <= 0 || b <= 0 || c <= 0 || a + b <= c || a + c <= b || b + c <= a = Illegal
  | a == b && b == c = Equilateral
  | a == b || a == c || b == c = Isosceles
  | otherwise = Scalene
