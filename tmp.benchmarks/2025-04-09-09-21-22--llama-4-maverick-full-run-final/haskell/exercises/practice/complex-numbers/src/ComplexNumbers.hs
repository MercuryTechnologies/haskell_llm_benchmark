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

-- Data definition -------------------------------------------------------------
data Complex a = C a a deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (r, i) = C r i

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (C r i) = C r (-i)

abs :: Floating a => Complex a -> a
abs (C r i) = sqrt (r^2 + i^2)

real :: Num a => Complex a -> a
real (C r _) = r

imaginary :: Num a => Complex a -> a
imaginary (C _ i) = i

exp :: Floating a => Complex a -> Complex a
exp (C r i) = C (eR * cos i) (eR * sin i)
  where eR = Prelude.exp r

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (C r1 i1) (C r2 i2) = C (r1*r2 - i1*i2) (r1*i2 + r2*i1)

add :: Num a => Complex a -> Complex a -> Complex a
add (C r1 i1) (C r2 i2) = C (r1 + r2) (i1 + i2)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (C r1 i1) (C r2 i2) = C (r1 - r2) (i1 - i2)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (C r1 i1) (C r2 i2) = C ((r1*r2 + i1*i2) / (r2^2 + i2^2)) ((i1*r2 - r1*i2) / (r2^2 + i2^2))
