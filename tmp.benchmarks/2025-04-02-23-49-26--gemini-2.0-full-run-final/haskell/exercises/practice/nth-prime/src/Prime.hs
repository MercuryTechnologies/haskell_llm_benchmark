module Prime (nth) where

import Data.Sequence (Seq, fromList, (|>))
import qualified Data.Sequence as Seq

nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing
  | otherwise = Just $ findNthPrime n

findNthPrime :: Int -> Integer
findNthPrime n = Seq.index primes (n - 1)
  where
    primes :: Seq Integer
    primes = sieve 2 (fromList [2..])

sieve :: Integer -> Seq Integer -> Seq Integer
sieve p candidates =
  case Seq.viewl candidates of
    EmptyL -> Seq.empty
    h Seq.:< t -> h |> sieve (Seq.index (h |< t) 0) (Seq.filter (\x -> x `rem` p /= 0) t)
