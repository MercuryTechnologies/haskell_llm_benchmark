module RationalNumbers
(Rational,
 abs,
 numerator,
 denominator,
 add,
 sub,
 mul,
 div,
 pow,
 expRational,
 expReal,
 rational) where

import Prelude hiding (div, abs, Rational)
import qualified Prelude as P (abs)

-- Data definition -------------------------------------------------------------
data Rational a = Rational { num :: a, den :: a } deriving(Eq, Show)

-- Helper function to compute greatest common divisor
gcd' :: Integral a => a -> a -> a
gcd' 0 b = P.abs b
gcd' a 0 = P.abs a
gcd' a b = gcd' (b `mod` a) a

-- Create a rational number in reduced form
rational :: Integral a => (a, a) -> Rational a
rational (n, d)
  | d == 0    = error "Denominator cannot be zero"
  | d < 0     = Rational (-n `div` g) (-d `div` g)  -- Ensure denominator is positive
  | otherwise = Rational (n `div` g) (d `div` g)
  where g = gcd' (P.abs n) (P.abs d)

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational n d) = Rational (P.abs n) d

numerator :: Integral a => Rational a -> a
numerator = num

denominator :: Integral a => Rational a -> a
denominator = den

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational n1 d1) (Rational n2 d2) = rational (n1 * d2 + n2 * d1, d1 * d2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational n1 d1) (Rational n2 d2) = rational (n1 * d2 - n2 * d1, d1 * d2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational n1 d1) (Rational n2 d2) = rational (n1 * n2, d1 * d2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational n1 d1) (Rational n2 d2)
  | n2 == 0   = error "Division by zero"
  | otherwise = rational (n1 * d2, d1 * n2)

pow :: Integral a => Rational a -> a -> Rational a
pow (Rational n d) e
  | e < 0     = rational (d ^ P.abs e, n ^ P.abs e)
  | otherwise = rational (n ^ e, d ^ e)

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (Rational n d) e = (fromIntegral n ** e) / (fromIntegral d ** e)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (Rational n d) = (x ** fromIntegral n) ** (1 / fromIntegral d)
