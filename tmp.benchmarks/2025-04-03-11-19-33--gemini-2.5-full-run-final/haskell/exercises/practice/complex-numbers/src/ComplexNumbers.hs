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
import qualified Prelude (exp) -- Use qualified import for Prelude's exp

-- Data definition -------------------------------------------------------------
-- Represents a complex number a + bi
data Complex a = Complex a a deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (r, i) = Complex r i

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex r i) = Complex r (-i)

abs :: Floating a => Complex a -> a
abs (Complex r i) = sqrt (r*r + i*i)

real :: Num a => Complex a -> a
real (Complex r _) = r

imaginary :: Num a => Complex a -> a
imaginary (Complex _ i) = i

-- e^(a + ib) = e^a * e^(ib) = e^a * (cos b + i * sin b)
exp :: Floating a => Complex a -> Complex a
exp (Complex a b) = Complex (Prelude.exp a * cos b) (Prelude.exp a * sin b)

-- binary operators ------------------------------------------------------------
-- (a + ib) * (c + id) = (ac - bd) + i(bc + ad)
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex a b) (Complex c d) = Complex (a*c - b*d) (b*c + a*d)

-- (a + ib) + (c + id) = (a + c) + i(b + d)
add :: Num a => Complex a -> Complex a -> Complex a
add (Complex a b) (Complex c d) = Complex (a+c) (b+d)

-- (a + ib) - (c + id) = (a - c) + i(b - d)
sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex a b) (Complex c d) = Complex (a-c) (b-d)

-- (a + ib) / (c + id) = (ac + bd)/(c^2 + d^2) + i(bc - ad)/(c^2 + d^2)
div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex a b) (Complex c d) =
  let denom = c*c + d*d
  in Complex ((a*c + b*d) / denom) ((b*c - a*d) / denom)
