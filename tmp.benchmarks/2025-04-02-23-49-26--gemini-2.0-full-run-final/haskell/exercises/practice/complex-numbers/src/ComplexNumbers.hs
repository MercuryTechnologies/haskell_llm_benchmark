module ComplexNumbers
(Complex,
 conjugate,
 abs,
 exp,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs, exp)
import qualified Prelude as P
-- import Data.Complex (Complex((:+)), realPart, imagPart)

-- Data definition -------------------------------------------------------------
data Complex a = C a a deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (a,b) = C a b

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (C a b) = C a (negate b)

abs :: Floating a => Complex a -> a
abs (C a b) = sqrt (a * a + b * b)

real :: Num a => Complex a -> a
real (C a b) = a

imaginary :: Num a => Complex a -> a
imaginary (C a b) = b

exp :: Floating a => Complex a -> Complex a
exp (C a b) = let
  e_a = P.exp a
  in C (e_a * P.cos b) (e_a * P.sin b)

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (C a b) (C c d) = C (a * c - b * d) (b * c + a * d)

add :: Num a => Complex a -> Complex a -> Complex a
add (C a b) (C c d) = C (a + c) (b + d)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (C a b) (C c d) = C (a - c) (b - d)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (C a b) (C c d) = let
  denominator = c * c + d * d
  in C ((a * c + b * d) / denominator) ((b * c - a * d) / denominator)
