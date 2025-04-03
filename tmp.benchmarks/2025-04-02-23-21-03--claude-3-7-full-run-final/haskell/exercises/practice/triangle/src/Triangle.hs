module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | not (isValidTriangle a b c) = Illegal
  | a == b && b == c            = Equilateral
  | a == b || b == c || a == c  = Isosceles
  | otherwise                   = Scalene
  where
    isValidTriangle x y z = 
      x > 0 && y > 0 && z > 0 && -- all sides must be positive
      x + y >= z && y + z >= x && x + z >= y -- triangle inequality
