module Poker (bestHands) where

import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.Map.Strict as Map

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show, Ord, Enum, Bounded)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Show, Ord, Enum, Bounded)

data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq, Show, Ord)
type Hand = [Card]

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
    deriving (Eq, Show, Ord)

bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hands =
    let parsed = map (\h -> (h, parseHand h)) hands
        valid = [ (h, ph) | (h, Just ph) <- parsed ]
        ranked = [ (h, handRank ph) | (h, ph) <- valid ]
        best = maximumByMay (comparing snd) ranked
        bestRank = fmap snd best
        winners = [ h | (h, r) <- ranked, Just r == bestRank ]
    in if null winners then Nothing else Just winners

maximumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumByMay _ [] = Nothing
maximumByMay cmp xs = Just (maximumBy cmp xs)

parseHand :: String -> Maybe Hand
parseHand s =
    let ws = words s
        cards = map parseCard ws
    in if length cards == 5 && all isJust cards
        then Just (map fromJust cards)
        else Nothing

parseCard :: String -> Maybe Card
parseCard [r,s] = Card <$> parseRank [r] <*> parseSuit s
parseCard ['1','0',s] = Card <$> parseRank "10" <*> parseSuit s
parseCard _ = Nothing

parseRank :: String -> Maybe Rank
parseRank "2" = Just Two
parseRank "3" = Just Three
parseRank "4" = Just Four
parseRank "5" = Just Five
parseRank "6" = Just Six
parseRank "7" = Just Seven
parseRank "8" = Just Eight
parseRank "9" = Just Nine
parseRank "10" = Just Ten
parseRank "J" = Just Jack
parseRank "Q" = Just Queen
parseRank "K" = Just King
parseRank "A" = Just Ace
parseRank _ = Nothing

parseSuit :: Char -> Maybe Suit
parseSuit 'C' = Just Clubs
parseSuit 'D' = Just Diamonds
parseSuit 'H' = Just Hearts
parseSuit 'S' = Just Spades
parseSuit _ = Nothing

handRank :: Hand -> HandRank
handRank hand
    | isStraightFlush = StraightFlush highStraight
    | isFourOfAKind   = FourOfAKind fourRank kicker
    | isFullHouse     = FullHouse threeRank pairRank
    | isFlush         = Flush ranksDesc
    | isStraight      = Straight highStraight
    | isThreeOfAKind  = ThreeOfAKind threeRank kickers
    | isTwoPair       = TwoPair highPair lowPair kicker
    | isOnePair       = OnePair pairRank kickers
    | otherwise       = HighCard ranksDesc
  where
    ranks = sortBy (flip compare) $ map rank hand
    ranksDesc = ranks
    suits = map suit hand
    grouped = sortBy (\a b -> compare (length b) (length a) <> compare (head b) (head a)) . group . sort $ ranks
    counts = map length grouped
    isFlush = all (== head suits) suits
    isStraight = isJust straightHigh
    isStraightFlush = isFlush && isStraight
    straightHigh = getStraightHigh ranks
    highStraight = fromJust straightHigh
    isFourOfAKind = counts == [4,1]
    isFullHouse = counts == [3,2]
    isThreeOfAKind = counts == [3,1,1]
    isTwoPair = counts == [2,2,1]
    isOnePair = counts == [2,1,1,1]
    fourRank = head (head [g | g <- grouped, length g == 4])
    threeRank = head (head [g | g <- grouped, length g == 3])
    pairRank = head (head [g | g <- grouped, length g == 2])
    pairs = [head g | g <- grouped, length g == 2]
    highPair = maximum pairs
    lowPair = minimum pairs
    kicker = head [head g | g <- grouped, length g == 1]
    kickers = concat [map head g | g <- [grouped], any ((==1) . length) g]

getStraightHigh :: [Rank] -> Maybe Rank
getStraightHigh rs =
    let uniq = reverse . sort . nub $ rs
        aceLow = [Ace, Five, Four, Three, Two]
        isConsecutive xs = and $ zipWith (\a b -> fromEnum a - 1 == fromEnum b) xs (tail xs)
    in case uniq of
        [Ace, King, Queen, Jack, Ten] -> Just Ace
        [King, Queen, Jack, Ten, Nine] -> Just King
        [Queen, Jack, Ten, Nine, Eight] -> Just Queen
        [Jack, Ten, Nine, Eight, Seven] -> Just Jack
        [Ten, Nine, Eight, Seven, Six] -> Just Ten
        [Nine, Eight, Seven, Six, Five] -> Just Nine
        [Eight, Seven, Six, Five, Four] -> Just Eight
        [Seven, Six, Five, Four, Three] -> Just Seven
        [Six, Five, Four, Three, Two] -> Just Six
        _ | uniq == aceLow -> Just Five
        _ -> Nothing
