module Poker (bestHands) where

import Data.Maybe (catMaybes)
import Data.List (group, sort, sortBy, maximumBy)
import Data.Ord (comparing)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Enum, Show)
data Suit = Hearts | Diamonds | Clubs | Spades deriving (Eq, Show)
data Hand = Hand { ranks :: [Rank], suits :: [Suit] } deriving (Show, Eq)

parseHand :: String -> Hand
parseHand hand = Hand (map rankValue ranks) (map suitValue suits)
  where
    (ranks, suits) = unzip [(c, drop 1 c) | c <- words hand]
    rankValue r = case r of
      "2" -> Two
      "3" -> Three
      "4" -> Four
      "5" -> Five
      "6" -> Six
      "7" -> Seven
      "8" -> Eight
      "9" -> Nine
      "10" -> Ten
      "J" -> Jack
      "Q" -> Queen
      "K" -> King
      "A" -> Ace
    suitValue s = case s of
      "H" -> Hearts
      "D" -> Diamonds
      "C" -> Clubs
      "S" -> Spades

bestHands :: [String] -> Maybe [String]
bestHands hands = Just $ map showHand best
  where
    parsedHands = map parseHand hands
    best = findBestHands parsedHands

findBestHands :: [Hand] -> [Hand]
findBestHands hands = filter isBest hands
  where
    isBest hand = hand == maximumBy (comparing rankValue) hands
    rankValue hand = sum (map fromEnum (ranks hand))

showHand :: Hand -> String
showHand (Hand ranks suits) = unwords [show r ++ show s | (r, s) <- zip ranks suits]
