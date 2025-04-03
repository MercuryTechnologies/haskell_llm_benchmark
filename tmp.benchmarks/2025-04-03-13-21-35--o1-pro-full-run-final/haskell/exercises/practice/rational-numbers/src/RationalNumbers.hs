module RationalNumbers
( Rational
, abs
, numerator
, denominator
, add
, sub
, mul
, div
, pow
, expRational
, expReal
, rational
) where

import Prelude hiding (div, abs, Rational)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Rational a = Dummy a a deriving(Eq, Show)

-- Helper function to reduce a pair (n, d) to standard form
reduce :: Integral a => a -> a -> (a, a)
reduce n d =
  if d == 0
  then P.error "Denominator cannot be zero"
  else
    let g = P.gcd n d
        n' = n `P.div` g
        d' = d `P.div` g
    in if d' < 0
       then (-n', -d')
       else (n', d')

rational :: Integral a => (a, a) -> Rational a
rational (n, d) =
  let (n', d') = reduce n d
  in Dummy n' d'

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Dummy n d) =
  let (n', d') = reduce (P.abs n) (P.abs d)
  in Dummy n' d'

numerator :: Integral a => Rational a -> a
numerator (Dummy n _) = n

denominator :: Integral a => Rational a -> a
denominator (Dummy _ d) = d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Dummy a b) (Dummy c d) =
  let (n', d') = reduce (a * d P.+ b * c) (b * d)
  in Dummy n' d'

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Dummy a b) (Dummy c d) =
  let (n', d') = reduce (a * d P.- b * c) (b * d)
  in Dummy n' d'

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Dummy a b) (Dummy c d) =
  let (n', d') = reduce (a * c) (b * d)
  in Dummy n' d'

div :: Integral a => Rational a -> Rational a -> Rational a
div (Dummy a b) (Dummy c d) =
  if c == 0
  then P.error "Division by zero"
  else
    let (n', d') = reduce (a * d) (b * c)
    in Dummy n' d'

pow :: Integral a => Rational a -> a -> Rational a
pow (Dummy n d) e
  | e P.== 0 = Dummy 1 1
  | e P.> 0  =
      let (n', d') = reduce (n P.^ e) (d P.^ e)
      in Dummy n' d'
  | otherwise =
      let e' = P.abs e
          (n', d') = reduce (d P.^ e') (n P.^ e')
      in Dummy n' d'

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (Dummy n d) x =
  (P.fromIntegral n P.** x) P./ (P.fromIntegral d P.** x)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (Dummy n d) =
  x P.** ((P.fromIntegral n) P./ (P.fromIntegral d))
