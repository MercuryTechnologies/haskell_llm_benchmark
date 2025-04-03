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
import Data.Ratio (Ratio, (%), approxRational)
import qualified Data.Ratio as Ratio
import Data.Maybe (fromMaybe)

-- Data definition -------------------------------------------------------------
type Rational a = Ratio a

rational :: Integral a => (a, a) -> Rational a
rational (num, den)
    | den == 0    = error "Denominator cannot be zero"
    | otherwise   = reduce (num % den)

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs r = abs (numerator r) % denominator r

numerator :: Integral a => Rational a -> a
numerator r = Ratio.numerator r

denominator :: Integral a => Rational a -> a
denominator r = Ratio.denominator r

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add r1 r2 = reduce $ r1 + r2

sub :: Integral a => Rational a -> Rational a -> Rational a
sub r1 r2 = reduce $ r1 - r2

mul :: Integral a => Rational a -> Rational a -> Rational a
mul r1 r2 = reduce $ r1 * r2

div :: Integral a => Rational a -> Rational a -> Rational a
div r1 r2
    | Ratio.numerator r2 == 0 = error "Cannot divide by zero"
    | otherwise         = reduce $ r1 / r2

pow :: Integral a => Rational a -> a -> Rational a
pow r n = reduce $ (numerator r)^n % (denominator r)^n

expRational :: (RealFloat b, Integral a) => Rational a -> b -> b
expRational r x = (fromIntegral (numerator r) ** x) / (fromIntegral (denominator r) ** x)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x r = x ** (fromIntegral (numerator r) / fromIntegral (denominator r))

-- Helper function to reduce a rational number to lowest terms and standard form
reduce :: Integral a => Rational a -> Rational a
reduce r = let
    n = numerator r
    d = denominator r
    g = gcd n d
    in if d >= 0
       then (n `div` g) % (d `div` g)
       else ((-1) * n `div` g) % ((-1) * d `div` g)
