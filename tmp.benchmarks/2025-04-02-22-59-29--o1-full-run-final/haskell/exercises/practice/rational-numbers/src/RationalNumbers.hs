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
import qualified Prelude as P (Rational, gcd, abs)

-- Data definition -------------------------------------------------------------
data Rational a = R a a deriving (Eq, Show)

-- Helper for normalizing a fraction -------------------------------------------
normalize :: (Integral a) => (a, a) -> (a, a)
normalize (n, d)
  | d == 0    = error "Denominator cannot be zero"
  | otherwise =
      let g = P.gcd n d
          s = if d < 0 then -1 else 1
      in (s * (n `div` g), s * (d `div` g))

-- Constructor -----------------------------------------------------------------
rational :: Integral a => (a, a) -> Rational a
rational (n, d) =
  let (n', d') = normalize (n, d)
  in R n' d'

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (R n d) =
  let (n', d') = normalize (P.abs n, P.abs d)
  in R n' d'

numerator :: Integral a => Rational a -> a
numerator (R n _) = n

denominator :: Integral a => Rational a -> a
denominator (R _ d) = d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (R n1 d1) (R n2 d2) =
  let (n', d') = normalize (n1 * d2 + n2 * d1, d1 * d2)
  in R n' d'

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (R n1 d1) (R n2 d2) =
  let (n', d') = normalize (n1 * d2 - n2 * d1, d1 * d2)
  in R n' d'

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (R n1 d1) (R n2 d2) =
  let (n', d') = normalize (n1 * n2, d1 * d2)
  in R n' d'

div :: Integral a => Rational a -> Rational a -> Rational a
div (R n1 d1) (R n2 d2)
  | n2 == 0   = error "Cannot divide by a rational number with numerator 0"
  | otherwise =
      let (n', d') = normalize (n1 * d2, d1 * n2)
      in R n' d'

pow :: Integral a => Rational a -> a -> Rational a
pow (R n d) e
  | e == 0    = R 1 1
  | e > 0     =
      let (n', d') = normalize (n ^ e, d ^ e)
      in R n' d'
  | otherwise =
      let e' = P.abs e
          (n', d') = normalize (d ^ e', n ^ e')
      in R n' d'

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (R n d) x =
  (fromIntegral n ** x) / (fromIntegral d ** x)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (R n d) =
  x ** (fromIntegral n / fromIntegral d)
