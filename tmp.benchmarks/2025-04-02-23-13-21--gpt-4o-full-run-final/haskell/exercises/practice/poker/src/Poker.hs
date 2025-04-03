module Poker (bestHands) where

import Data.List (sort, group, sortBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Eq, Show)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Enum, Show)
data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq, Show)
data HandRank = HighCard [Rank] | OnePair Rank [Rank] | TwoPair Rank Rank [Rank] | ThreeOfAKind Rank [Rank] | Straight Rank | Flush [Rank] | FullHouse Rank Rank | FourOfAKind Rank Rank | StraightFlush Rank deriving (Eq, Show, Ord)

parseCard :: String -> Card
parseCard [r, s] = Card (parseRank r) (parseSuit s)
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
    parseRank _   = error "Invalid rank"
    
    parseSuit 'H' = Hearts
    parseSuit 'D' = Diamonds
    parseSuit 'C' = Clubs
    parseSuit 'S' = Spades
    parseSuit _   = error "Invalid suit"
parseCard _ = error "Invalid card format"

parseHand :: String -> [Card]
parseHand = map parseCard . words

handRank :: [Card] -> HandRank
handRank cards
  | isStraight && isFlush = StraightFlush highCard
  | isFourOfAKind         = FourOfAKind (head fourOfAKind) (head kicker)
  | isFullHouse           = FullHouse (head threeOfAKind) (head pair)
  | isFlush               = Flush ranks
  | isStraight            = Straight highCard
  | isThreeOfAKind        = ThreeOfAKind (head threeOfAKind) kickers
  | isTwoPair             = TwoPair (head highPair) (head lowPair) kickers
  | isOnePair             = OnePair (head pair) kickers
  | otherwise             = HighCard ranks
  where
    ranks = sort $ map rank cards
    suits = map suit cards
    isFlush = length (group suits) == 1
    isStraight = and $ zipWith (==) (tail ranks) (map succ (init ranks))
    highCard = last ranks
    groupedRanks = sortBy (comparing (negate . length)) . group $ ranks
    isFourOfAKind = length (head groupedRanks) == 4
    isFullHouse = length (head groupedRanks) == 3 && length (groupedRanks !! 1) == 2
    isThreeOfAKind = length (head groupedRanks) == 3
    isTwoPair = length (head groupedRanks) == 2 && length (groupedRanks !! 1) == 2
    isOnePair = length (head groupedRanks) == 2
    fourOfAKind = head groupedRanks
    threeOfAKind = head groupedRanks
    pair = head groupedRanks
    highPair = head groupedRanks
    lowPair = groupedRanks !! 1
    kickers = concat $ drop 1 groupedRanks
    kicker = concat $ drop 2 groupedRanks

bestHands :: [String] -> Maybe [String]
bestHands hands = Just $ map snd $ filter ((== bestRank) . fst) rankedHands
  where
    parsedHands = map parseHand hands
    rankedHands = zip (map handRank parsedHands) hands
    bestRank = maximum $ map fst rankedHands
