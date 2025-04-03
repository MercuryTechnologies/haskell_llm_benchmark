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
import qualified Prelude as P (abs, gcd)

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving (Eq, Show)

-- Helper function to reduce a rational number to its lowest terms
reduce :: Integral a => Rational a -> Rational a
reduce (Rational n d) =
    let g = P.gcd n d
        (n', d') = (n `div` g, d `div` g)
    in if d' < 0 then Rational (-n') (-d') else Rational n' d'

rational :: Integral a => (a, a) -> Rational a
rational (n, d)
    | d == 0    = error "Denominator cannot be zero"
    | otherwise = reduce (Rational n d)

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational n d) = Rational (P.abs n) (P.abs d)

numerator :: Integral a => Rational a -> a
numerator (Rational n _) = n

denominator :: Integral a => Rational a -> a
denominator (Rational _ d) = d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational n1 d1) (Rational n2 d2) = reduce $ Rational (n1 * d2 + n2 * d1) (d1 * d2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational n1 d1) (Rational n2 d2) = reduce $ Rational (n1 * d2 - n2 * d1) (d1 * d2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational n1 d1) (Rational n2 d2) = reduce $ Rational (n1 * n2) (d1 * d2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational n1 d1) (Rational n2 d2)
    | n2 == 0   = error "Cannot divide by zero"
    | otherwise = reduce $ Rational (n1 * d2) (d1 * n2)

pow :: Integral a => Rational a -> a -> Rational a
pow (Rational n d) e
    | e >= 0    = reduce $ Rational (n ^ e) (d ^ e)
    | otherwise = reduce $ Rational (d ^ P.abs e) (n ^ P.abs e)

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (Rational n d) x = (fromIntegral n ** x) / (fromIntegral d ** x)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (Rational n d) = x ** (fromIntegral n / fromIntegral d)
