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

-- Data definition -------------------------------------------------------------
data Complex a = Complex a a deriving (Eq, Show)

complex :: (a, a) -> Complex a
complex (re, im) = Complex re im

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex re im) = Complex re (negate im)

abs :: Floating a => Complex a -> a
abs (Complex re im) = sqrt (re * re + im * im)

real :: Num a => Complex a -> a
real (Complex re _) = re

imaginary :: Num a => Complex a -> a
imaginary (Complex _ im) = im

exp :: Floating a => Complex a -> Complex a
exp (Complex re im) =
    let eRe = P.exp re
    in Complex (eRe * cos im) (eRe * sin im)

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex a b) (Complex c d) =
    Complex (a * c - b * d) (b * c + a * d)

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex a b) (Complex c d) =
    Complex (a + c) (b + d)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex a b) (Complex c d) =
    Complex (a - c) (b - d)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex a b) (Complex c d) =
    let denom = c * c + d * d
    in Complex ((a * c + b * d) / denom) ((b * c - a * d) / denom)
