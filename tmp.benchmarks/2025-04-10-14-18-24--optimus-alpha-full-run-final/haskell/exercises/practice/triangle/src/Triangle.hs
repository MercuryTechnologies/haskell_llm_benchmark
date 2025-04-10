module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | not isValid = Illegal
  | a == b && b == c = Equilateral
  | a == b || b == c || a == c = Isosceles
  | otherwise = Scalene
  where
    sides = [a, b, c]
    allPositive = all (> 0) sides
    satisfiesInequality = a + b >= c && b + c >= a && a + c >= b
    isValid = allPositive && satisfiesInequality
