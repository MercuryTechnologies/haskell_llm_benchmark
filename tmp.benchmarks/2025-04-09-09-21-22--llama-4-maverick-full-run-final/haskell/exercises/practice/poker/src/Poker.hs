module Poker (bestHands) where

import Data.List (sort, sortBy, maximumBy, groupBy)
import Data.Ord (comparing)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Show)

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Eq, Show)

data Card = Card Rank Suit
  deriving (Eq, Show)

data HandType = HighCard | OnePair | TwoPairs | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
  deriving (Eq, Ord, Show)

parseCard :: String -> Card
parseCard (r:rs) = Card (parseRank r) (parseSuit (last rs))
  where
    parseRank '2' = Two
    parseRank '3' = Three
    parseRank '4' = Four
    parseRank '5' = Five
    parseRank '6' = Six
    parseRank '7' = Seven
    parseRank '8' = Eight
    parseRank '9' = Nine
    parseRank 'T' = Ten
    parseRank 'J' = Jack
    parseRank 'Q' = Queen
    parseRank 'K' = King
    parseRank 'A' = Ace
    parseRank c = error $ "Invalid rank: " ++ [c]
    parseSuit 'H' = Hearts
    parseSuit 'D' = Diamonds
    parseSuit 'C' = Clubs
    parseSuit 'S' = Spades
    parseSuit c = error $ "Invalid suit: " ++ [c]

parseHand :: String -> [Card]
parseHand = map parseCard . words

evaluateHand :: [Card] -> (HandType, [Rank])
evaluateHand cards = (handType, sortedRanks)
  where
    sortedCards = sortBy (comparing (\(Card r _) -> r)) cards
    sortedRanks = map (\(Card r _) -> r) sortedCards
    isFlush = all (\(Card _ s) -> s == suit (head cards)) cards
      where
        suit (Card _ s) = s
    isStraight = and $ zipWith (\a b -> succ a == b) (init sortedRanks) (tail sortedRanks)
    rankCounts = countRanks sortedRanks
    handType
      | isStraight && isFlush && head sortedRanks == Ace = RoyalFlush
      | isStraight && isFlush = StraightFlush
      | FourOfAKind `elem` map fst rankCounts = FourOfAKind
      | ThreeOfAKind `elem` map fst rankCounts && OnePair `elem` map fst rankCounts = FullHouse
      | isFlush = Flush
      | isStraight = Straight
      | ThreeOfAKind `elem` map fst rankCounts = ThreeOfAKind
      | length (filter ((== OnePair) . fst) rankCounts) == 2 = TwoPairs
      | OnePair `elem` map fst rankCounts = OnePair
      | otherwise = HighCard
    countRanks = map (\rs -> (toHandType (length rs), head rs)) . groupBy (==) . sort
    toHandType 2 = OnePair
    toHandType 3 = ThreeOfAKind
    toHandType 4 = FourOfAKind
    toHandType _ = error "Invalid count"

bestHands :: [String] -> Maybe [String]
bestHands hands
  | null hands = Nothing
  | otherwise = Just $ filter (\h -> evaluateHand (parseHand h) == maxHand) hands
  where
    handValues = map (\h -> (h, evaluateHand (parseHand h))) hands
    maxHand = maximum $ map snd handValues
