module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | any (<= 0) [a, b, c] = Illegal
  | not (isValidTriangle a b c) = Illegal
  | a == b && b == c = Equilateral
  | a == b || b == c || a == c = Isosceles
  | otherwise = Scalene

isValidTriangle :: (Num a, Ord a) => a -> a -> a -> Bool
isValidTriangle a b c =
  let ab = a + b
      bc = b + c
      ac = a + c
  in ab >= c && bc >= a && ac >= b
