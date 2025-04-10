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
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving (Eq)

instance (Show a, Integral a) => Show (Rational a) where
    show r = show (numerator r) ++ "/" ++ show (denominator r)

-- Helper function to compute gcd and reduce to lowest terms and standard form
reduce :: Integral a => a -> a -> (a, a)
reduce n d
    | d == 0    = error "Denominator cannot be zero"
    | d < 0     = let g = P.gcd n d in ((-n) `quot` g, (-d) `quot` g)
    | otherwise = let g = P.gcd n d in (n `quot` g, d `quot` g)

-- Smart constructor
rational :: Integral a => (a, a) -> Rational a
rational (n, d) =
    let (rn, rd) = reduce n d
    in Rational rn rd

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational n d) = Rational (P.abs n) (P.abs d)

numerator :: Integral a => Rational a -> a
numerator (Rational n _) = n

denominator :: Integral a => Rational a -> a
denominator (Rational _ d) = d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational n1 d1) (Rational n2 d2) =
    rational (n1 * d2 + n2 * d1, d1 * d2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational n1 d1) (Rational n2 d2) =
    rational (n1 * d2 - n2 * d1, d1 * d2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational n1 d1) (Rational n2 d2) =
    rational (n1 * n2, d1 * d2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational n1 d1) (Rational n2 d2)
    | n2 == 0   = error "Division by zero"
    | otherwise = rational (n1 * d2, d1 * n2)

pow :: Integral a => Rational a -> a -> Rational a
pow (Rational n d) e
    | e == 0    = Rational 1 1
    | e > 0     = rational (n ^ e, d ^ e)
    | otherwise = rational (d ^ (P.abs e), n ^ (P.abs e))

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (Rational n d) x = (fromIntegral n ** x) / (fromIntegral d ** x)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (Rational n d) = x ** (fromIntegral n / fromIntegral d)
