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

-- Data definition -------------------------------------------------------------
data Rational a = Rational !a !a deriving(Eq)

instance (Show a) => Show (Rational a) where
  show (Rational a b) = show a ++ "/" ++ show b

rational :: Integral a => (a, a) -> Rational a
rational (a, b) 
  | b == 0 = error "Denominator cannot be zero"
  | otherwise = let g = gcd a b
                    sign = if b < 0 then -1 else 1
                in Rational (sign * (a `quot` g)) (sign * (b `quot` g))

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational a b) = Rational (abs a) b

numerator :: Integral a => Rational a -> a
numerator (Rational a _) = a

denominator :: Integral a => Rational a -> a
denominator (Rational _ b) = b

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational a1 b1) (Rational a2 b2) = rational (a1 * b2 + a2 * b1, b1 * b2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational a1 b1) (Rational a2 b2) = rational (a1 * b2 - a2 * b1, b1 * b2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational a1 b1) (Rational a2 b2) = rational (a1 * a2, b1 * b2)

div :: Integral a => Rational a -> Rational a -> Rational a
div _ (Rational 0 _) = error "Cannot divide by zero"
div (Rational a1 b1) (Rational a2 b2) = rational (a1 * b2, a2 * b1)

pow :: Integral a => Rational a -> Integer -> Rational a
pow r 0 = rational (1, 1)
pow (Rational a b) n 
  | n > 0 = rational (a^n, b^n)
  | otherwise = rational (b^m, a^m)
  where m = abs n

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (Rational a b) x = (fromIntegral a / fromIntegral b) ** x

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (Rational a b) = x ** (fromIntegral a / fromIntegral b)
