module Poker (bestHands) where

import Data.List (group, sort, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Enum, Bounded, Show)

data Card = Card Rank Suit deriving (Eq, Show)
type Hand = [Card]

bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hands =
    let parsedHands = mapMaybe parseHand hands
        rankedHands = map (\h -> (handRank h, h)) parsedHands
        bestRank = maximum (map fst rankedHands)
        bests = [original | (r, hnd) <- rankedHands, r == bestRank, let original = handToString hnd]
    in Just bests

parseHand :: String -> Maybe Hand
parseHand s = sequence $ map parseCard (words s)

parseCard :: String -> Maybe Card
parseCard [r,s] = Card <$> parseRank r <*> parseSuit s
parseCard ['1','0',s] = Card <$> Just Ten <*> parseSuit s
parseCard _ = Nothing

parseRank :: Char -> Maybe Rank
parseRank '2' = Just Two
parseRank '3' = Just Three
parseRank '4' = Just Four
parseRank '5' = Just Five
parseRank '6' = Just Six
parseRank '7' = Just Seven
parseRank '8' = Just Eight
parseRank '9' = Just Nine
parseRank 'J' = Just Jack
parseRank 'Q' = Just Queen
parseRank 'K' = Just King
parseRank 'A' = Just Ace
parseRank _ = Nothing

parseSuit :: Char -> Maybe Suit
parseSuit 'C' = Just Clubs
parseSuit 'D' = Just Diamonds
parseSuit 'H' = Just Hearts
parseSuit 'S' = Just Spades
parseSuit _ = Nothing

handToString :: Hand -> String
handToString = unwords . map cardToString

cardToString :: Card -> String
cardToString (Card r s) = rankToString r ++ suitToString s

rankToString :: Rank -> String
rankToString Ten = "10"
rankToString Two = "2"
rankToString Three = "3"
rankToString Four = "4"
rankToString Five = "5"
rankToString Six = "6"
rankToString Seven = "7"
rankToString Eight = "8"
rankToString Nine = "9"
rankToString Jack = "J"
rankToString Queen = "Q"
rankToString King = "K"
rankToString Ace = "A"

suitToString :: Suit -> String
suitToString Clubs = "C"
suitToString Diamonds = "D"
suitToString Hearts = "H"
suitToString Spades = "S"

data HandRank
    = HighCard [Rank]
    | OnePair Rank [Rank]
    | TwoPair Rank Rank Rank
    | ThreeOfAKind Rank [Rank]
    | Straight Rank
    | Flush [Rank]
    | FullHouse Rank Rank
    | FourOfAKind Rank Rank
    | StraightFlush Rank
    deriving (Eq, Ord, Show)

handRank :: Hand -> HandRank
handRank hand
    | isStraightFlush = StraightFlush highStraight
    | isFourKind      = FourOfAKind fourRank kicker
    | isFullHouse     = FullHouse threeRank fullHousePairRank
    | isFlush         = Flush ranks
    | isStraight      = Straight highStraight
    | isThreeKind     = ThreeOfAKind threeRank threeKindKickers
    | isTwoPair       = TwoPair highPair lowPair kicker2
    | isOnePair       = OnePair pairRank pairKickers
    | otherwise       = HighCard ranks
  where
    ranks = reverse $ sort $ map (\(Card r _) -> r) hand
    suits = map (\(Card _ s) -> s) hand
    grouped = reverse $ sortBy (comparing length) . group . sort $ ranks

    isFlush = all (== head suits) suits
    isStraight = isConsecutive ranks || ranks == [Ace, Five, Four, Three, Two]
    isStraightFlush = isFlush && isStraight

    highStraight
      | ranks == [Ace, Five, Four, Three, Two] = Five
      | otherwise = maximum ranks

    isFourKind = case grouped of
        [g1, _] -> length g1 == 4
        _ -> False
    fourRank = head (head grouped)
    kicker = head (grouped !! 1)

    isFullHouse = case grouped of
        [g1, g2] -> length g1 == 3 && length g2 == 2
        _ -> False
    threeRank = head (head grouped)
    fullHousePairRank = head (grouped !! 1)

    isThreeKind = case grouped of
        [g1, g2, g3] -> length g1 == 3 && length g2 == 1 && length g3 == 1
        _ -> False
    threeKindKickers = map head (tail grouped)

    isTwoPair = case grouped of
        [g1, g2, g3] -> length g1 == 2 && length g2 == 2 && length g3 == 1
        _ -> False
    highPair = max (head (head grouped)) (head (head (tail grouped)))
    lowPair = min (head (head grouped)) (head (head (tail grouped)))
    kicker2 = head (grouped !! 2)

    isOnePair = case grouped of
        [g1, g2, g3, g4] -> length g1 == 2
        _ -> False
    pairRank = head (head grouped)
    pairKickers = concatMap (take 1) (tail grouped)

isConsecutive :: [Rank] -> Bool
isConsecutive rs = and $ zipWith (\a b -> fromEnum a - 1 == fromEnum b) rs (tail rs)
