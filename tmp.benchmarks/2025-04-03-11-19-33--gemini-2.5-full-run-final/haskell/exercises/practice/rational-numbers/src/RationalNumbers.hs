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
import qualified Prelude -- Use qualified import for Prelude functions like abs, gcd

-- Data definition -------------------------------------------------------------
-- Use RecordWildCards later if needed, but explicit access is fine for now.
data Rational a = Rational { num :: a, den :: a } deriving(Eq)

-- Show instance for easier debugging/testing if needed (optional based on tests)
instance (Integral a, Show a) => Show (Rational a) where
    show r = show (num r) ++ "/" ++ show (den r)

-- Constructor and normalization function --------------------------------------
rational :: Integral a => (a, a) -> Rational a
rational (n, d)
    | d == 0 = error "Denominator cannot be zero"
    | otherwise = let commonDivisor = Prelude.gcd n d
                      n' = n `Prelude.div` commonDivisor
                      d' = d `Prelude.div` commonDivisor
                  in if d' < 0
                     then Rational (-n') (-d') -- Ensure denominator is positive
                     else Rational n' d'

-- Unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs r = rational (Prelude.abs (num r), den r) -- Denominator is already positive

numerator :: Integral a => Rational a -> a
numerator = num

denominator :: Integral a => Rational a -> a
denominator = den

-- Binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add r1 r2 = rational (n1 * d2 + n2 * d1, d1 * d2)
  where
    n1 = num r1
    d1 = den r1
    n2 = num r2
    d2 = den r2

sub :: Integral a => Rational a -> Rational a -> Rational a
sub r1 r2 = rational (n1 * d2 - n2 * d1, d1 * d2)
  where
    n1 = num r1
    d1 = den r1
    n2 = num r2
    d2 = den r2

mul :: Integral a => Rational a -> Rational a -> Rational a
mul r1 r2 = rational (n1 * n2, d1 * d2)
  where
    n1 = num r1
    d1 = den r1
    n2 = num r2
    d2 = den r2

div :: Integral a => Rational a -> Rational a -> Rational a
div r1 r2
    | n2 == 0 = error "Division by zero rational number"
    | otherwise = rational (n1 * d2, d1 * n2)
  where
    n1 = num r1
    d1 = den r1
    n2 = num r2
    d2 = den r2

-- Exponentiation operators ----------------------------------------------------
-- Changed Int to Integer in the type signature to match test expectations
pow :: Integral a => Rational a -> Integer -> Rational a
pow r n
    | n > 0 = rational (num r ^ n, den r ^ n)
    | n == 0 = rational (1, 1) -- r^0 = 1
    | otherwise = -- n < 0
        if num r == 0
        then error "Division by zero: 0 raised to a negative power"
        else let m = Prelude.abs n
             in rational (den r ^ m, num r ^ m)

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational r x = (fromIntegral (num r) ** x) / (fromIntegral (den r) ** x)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x r = x ** (fromIntegral (num r) / fromIntegral (den r))
