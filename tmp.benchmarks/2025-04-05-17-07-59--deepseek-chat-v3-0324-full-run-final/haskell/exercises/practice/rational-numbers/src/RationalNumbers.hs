module RationalNumbers
(RationalNumber,
 absVal,
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

import Prelude hiding (div, abs)
import Data.Ratio (numerator, denominator, (%))
import GHC.Real (Ratio(..))

-- Data definition -------------------------------------------------------------
type RationalNumber a = Ratio a

rational :: Integral a => (a, a) -> RationalNumber a
rational (n, d)
    | d == 0 = error "Denominator cannot be zero"
    | d < 0 = rational (-n, -d)  -- Ensure denominator is positive
    | otherwise = reduced
    where 
        reduced = (n `div` g) % (d `div` g)
        g = gcd n d

-- unary operators -------------------------------------------------------------
absVal :: Integral a => RationalNumber a -> RationalNumber a
absVal r = rational (abs (numerator r), denominator r)

-- binary operators ------------------------------------------------------------
add :: Integral a => RationalNumber a -> RationalNumber a -> RationalNumber a
add r1 r2 = rational (n1 * d2 + n2 * d1, d1 * d2)
    where
        n1 = numerator r1
        d1 = denominator r1
        n2 = numerator r2
        d2 = denominator r2

sub :: Integral a => RationalNumber a -> RationalNumber a -> RationalNumber a
sub r1 r2 = rational (n1 * d2 - n2 * d1, d1 * d2)
    where
        n1 = numerator r1
        d1 = denominator r1
        n2 = numerator r2
        d2 = denominator r2

mul :: Integral a => RationalNumber a -> RationalNumber a -> RationalNumber a
mul r1 r2 = rational (n1 * n2, d1 * d2)
    where
        n1 = numerator r1
        d1 = denominator r1
        n2 = numerator r2
        d2 = denominator r2

div :: Integral a => RationalNumber a -> RationalNumber a -> RationalNumber a
div r1 r2 = rational (n1 * d2, n2 * d1)
    where
        n1 = numerator r1
        d1 = denominator r1
        n2 = numerator r2
        d2 = denominator r2

pow :: Integral a => RationalNumber a -> a -> RationalNumber a
pow r n
    | n >= 0 = rational (num ^ n, denom ^ n)
    | otherwise = rational (denom ^ m, num ^ m)
    where
        num = numerator r
        denom = denominator r
        m = abs n

expRational :: Integral a => Floating b => RationalNumber a -> b -> b
expRational r x = (fromIntegral (numerator r) ** x) / (fromIntegral (denominator r) ** x)

expReal :: Floating a => Integral b => a -> RationalNumber b -> a
expReal x r = x ** (fromIntegral (numerator r) / fromIntegral (denominator r))
