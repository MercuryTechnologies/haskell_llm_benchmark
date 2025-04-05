module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | not validTriangle = Illegal
  | a == b && b == c  = Equilateral
  | a == b || b == c || a == c = Isosceles
  | otherwise         = Scalene
  where
    positiveSides = all (> 0) [a, b, c]
    satisfiesInequality = a + b >= c && b + c >= a && a + c >= b
    validTriangle = positiveSides && satisfiesInequality
