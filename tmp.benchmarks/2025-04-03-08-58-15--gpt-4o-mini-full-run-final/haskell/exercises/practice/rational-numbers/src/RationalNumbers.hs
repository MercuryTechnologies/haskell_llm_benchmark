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
import Data.List (genericTake)
import Data.Ratio (Ratio, (%))
import Numeric (showFFloat)

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving (Eq, Show)

gcd' :: Integral a => a -> a -> a
gcd' 0 b = abs b
gcd' a 0 = abs a
gcd' a b = gcd' b (a `mod` b)

reduce :: Integral a => Rational a -> Rational a
reduce (Rational num den) =
    let divisor = gcd' num den
        newNum = num `div` divisor
        newDen = den `div` divisor
    in if newDen < 0 then Rational (-newNum) (-newDen) else Rational newNum newDen

rational :: Integral a => (a, a) -> Rational a
rational (num, 0) = error "Denominator cannot be zero"
rational (num, den) = reduce (Rational num den)

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational num den) = reduce (Rational (abs num) (abs den))

numerator :: Integral a => Rational a -> a
numerator (Rational num _) = num

denominator :: Integral a => Rational a -> a
denominator (Rational _ den) = den

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational a1 b1) (Rational a2 b2) = reduce (Rational (a1 * b2 + a2 * b1) (b1 * b2))

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational a1 b1) (Rational a2 b2) = reduce (Rational (a1 * b2 - a2 * b1) (b1 * b2))

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational a1 b1) (Rational a2 b2) = reduce (Rational (a1 * a2) (b1 * b2))

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational a1 b1) (Rational a2 b2) =
    if a2 == 0 then error "Cannot divide by zero"
    else reduce (Rational (a1 * b2) (a2 * b1))

pow :: Integral a => Rational a -> a -> Rational a
pow (Rational num den) exp =
    let numPow = num ^ exp
        denPow = den ^ exp
    in reduce (Rational numPow denPow)

expRational :: (Floating b, Integral a) => Rational a -> b -> b
expRational (Rational num den) x = (fromIntegral num ** x) / (fromIntegral den ** x)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (Rational num den) = root (x ** fromIntegral num) (fromIntegral den)

root :: (Floating a) => a -> Int -> a
root x n = x ** (1 / fromIntegral n)
