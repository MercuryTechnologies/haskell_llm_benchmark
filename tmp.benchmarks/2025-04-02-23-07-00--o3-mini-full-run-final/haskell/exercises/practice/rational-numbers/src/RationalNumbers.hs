module RationalNumbers
    ( Rational,
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
      rational
    ) where

import Prelude hiding (div, abs, Rational)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Rational a = Rat a a deriving (Eq, Show)

-- Helper function to normalize a rational number into standard form.
normalize :: Integral a => a -> a -> Rational a
normalize _ 0 = error "Denominator cannot be zero"
normalize num denom =
    let factor = gcd num denom
        n = num `div` factor
        d = denom `div` factor
    in if d < 0
       then Rat (-n) (-d)
       else Rat n d

rational :: Integral a => (a, a) -> Rational a
rational (num, denom) = normalize num denom

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rat a b) = Rat (P.abs a) b  -- denominator is positive by normalization

numerator :: Integral a => Rational a -> a
numerator (Rat a _) = a

denominator :: Integral a => Rational a -> a
denominator (Rat _ b) = b

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rat a b) (Rat c d) = normalize (a * d + c * b) (b * d)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rat a b) (Rat c d) = normalize (a * d - c * b) (b * d)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rat a b) (Rat c d) = normalize (a * c) (b * d)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rat a b) (Rat c d)
    | c == 0    = error "Division by zero rational"
    | otherwise = normalize (a * d) (b * c)

pow :: Integral a => Rational a -> a -> Rational a
pow (Rat a b) n
    | n == 0    = Rat 1 1
    | n > 0     = normalize (a ^ n) (b ^ n)
    | otherwise =
        if a == 0 then error "Zero cannot be raised to a negative power"
        else normalize (b ^ m) (a ^ m)
    where m = P.abs n

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (Rat a b) x = (fromIntegral a ** x) / (fromIntegral b ** x)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (Rat a b) = (x ** fromIntegral a) ** (1 / fromIntegral b)
