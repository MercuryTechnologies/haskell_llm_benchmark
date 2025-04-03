module Poker (bestHands) where

import Data.List (group, sort, sortBy)
import Data.Maybe (fromMaybe, listToMaybe)

-- Data types
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Show, Enum)
data Suit = Club | Diamond | Heart | Spade deriving (Eq, Ord, Show, Enum)
data Card = Card Rank Suit deriving (Eq, Ord, Show)
data HandRank = HighCard Rank
              | Pair Rank
              | TwoPairs Rank Rank
              | ThreeOfAKind Rank
              | Straight Rank
              | Flush Suit
              | FullHouse Rank Rank
              | FourOfAKind Rank
              | StraightFlush Suit Rank
              | RoyalFlush Suit deriving (Eq, Ord, Show)

-- Parsing functions
parseRank :: Char -> Maybe Rank
parseRank '2' = Just Two
parseRank '3' = Just Three
parseRank '4' = Just Four
parseRank '5' = Just Five
parseRank '6' = Just Six
parseRank '7' = Just Seven
parseRank '8' = Just Eight
parseRank '9' = Just Nine
parseRank 'T' = Just Ten
parseRank 'J' = Just Jack
parseRank 'Q' = Just Queen
parseRank 'K' = Just King
parseRank 'A' = Just Ace
parseRank _   = Nothing

parseSuit :: Char -> Maybe Suit
parseSuit 'C' = Just Club
parseSuit 'D' = Just Diamond
parseSuit 'H' = Just Heart
parseSuit 'S' = Just Spade
parseSuit _   = Nothing

parseCard :: String -> Maybe Card
parseCard [rankChar, suitChar] = do
  rank <- parseRank rankChar
  suit <- parseSuit suitChar
  return (Card rank suit)
parseCard _ = Nothing

parseHand :: String -> Maybe [Card]
parseHand handString = do
  cards <- mapM parseCard (words handString)
  if length cards == 5 then return cards else Nothing

-- Hand evaluation functions

rankCounts :: [Card] -> [(Int, Rank)]
rankCounts cards = map (\g -> (length g, cardRank (head g))) $ group $ sort cards
  where cardRank (Card rank _) = rank

isStraight :: [Card] -> Maybe Rank
isStraight cards =
  let ranks = map (\(Card r _) -> r) $ sort cards
      ranksAsInts = map fromEnum ranks
      isConsecutive = all (==1) $ zipWith (-) (tail ranksAsInts) (init ranksAsInts)
  in if isConsecutive
     then return (last ranks)
     else if ranks == [Two, Three, Four, Five, Ace]
          then return Five
          else Nothing

isFlush :: [Card] -> Maybe Suit
isFlush cards =
  let suits = map (\(Card _ s) -> s) cards
  in if all (== head suits) suits
     then return (head suits)
     else Nothing

evaluateHand :: [Card] -> HandRank
evaluateHand cards =
  let counts = rankCounts cards
      flush = isFlush cards
      straight = isStraight cards
  in case (counts, straight, flush) of
    ([(5, r)], _, _) -> HighCard r
    ([(4, r1), (1, r2)], _, _) -> Pair r1
    ([(3, r1), (2, r2)], _, _) -> TwoPairs r1 r2
    ([(3, r), (1, _), (1, _)], _, _) -> ThreeOfAKind r
    (_, Just r, _) -> Straight r
    (_, _, Just s) -> Flush s
    ([(3, r1), (2, r2)], _, _) -> FullHouse r1 r2
    ([(4, r), (1, _)], _, _) -> FourOfAKind r
    ( _, Just r, Just s) -> if r == Ace then RoyalFlush s else StraightFlush s r
    _ -> HighCard Two -- Should not happen, but avoids incomplete pattern matching

compareHands :: [Card] -> [Card] -> Ordering
compareHands hand1 hand2 = compare (evaluateHand hand1) (evaluateHand hand2)

-- Best hands function
bestHands :: [String] -> Maybe [String]
bestHands handStrings = do
  hands <- mapM parseHand handStrings
  if null hands then return [] else do
    let evaluatedHands = zip handStrings $ map evaluateHand hands
        bestRank = maximum (map snd evaluatedHands)
        bestHandStrings = map fst $ filter (\(_, rank) -> rank == bestRank) evaluatedHands
    return bestHandStrings
