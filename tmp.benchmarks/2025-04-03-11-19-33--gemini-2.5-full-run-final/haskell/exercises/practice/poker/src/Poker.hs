module Poker (bestHands) where

import Data.List (groupBy, sortOn)
import Data.Ord (comparing, Down(..))
-- Removed unused imports: Data.Maybe, Control.Applicative, nub, sortBy

-- Data Types

data Suit = S | H | D | C deriving (Eq, Ord, Show, Enum, Bounded)
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | RT | RJ | RQ | RK | RA
    deriving (Eq, Ord, Show, Enum, Bounded)

data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq, Show)

-- Make Card Ord based on Rank only for sorting within a hand
instance Ord Card where
    compare = comparing rank

-- Represents a hand internally, always sorted descending by rank
newtype Hand = Hand { getCards :: [Card] } deriving (Eq, Show)

-- Hand Ranking definition, ordered from highest to lowest
data HandRank = StraightFlush Rank         -- Highest card rank
              | FourOfAKind Rank Rank      -- Rank of the four, rank of the kicker
              | FullHouse Rank Rank        -- Rank of the three, rank of the pair
              | Flush [Rank]               -- Ranks of cards, descending
              | Straight Rank              -- Highest card rank
              | ThreeOfAKind Rank [Rank]   -- Rank of the three, ranks of kickers descending
              | TwoPair Rank Rank Rank     -- Higher pair rank, lower pair rank, kicker rank
              | OnePair Rank [Rank]        -- Pair rank, kickers ranks descending
              | HighCard [Rank]            -- Ranks of cards, descending
              deriving (Eq, Ord, Show)

-- Parsing

parseSuit :: Char -> Maybe Suit
parseSuit 'S' = Just S
parseSuit 'H' = Just H
parseSuit 'D' = Just D
parseSuit 'C' = Just C
parseSuit _   = Nothing

parseRank :: Char -> Maybe Rank
parseRank '2' = Just R2
parseRank '3' = Just R3
parseRank '4' = Just R4
parseRank '5' = Just R5
parseRank '6' = Just R6
parseRank '7' = Just R7
parseRank '8' = Just R8
parseRank '9' = Just R9
parseRank 'T' = Just RT
parseRank 'J' = Just RJ
parseRank 'Q' = Just RQ
parseRank 'K' = Just RK
parseRank 'A' = Just RA
parseRank _   = Nothing

parseCard :: String -> Maybe Card
parseCard [r, s] = Card <$> parseRank r <*> parseSuit s
parseCard ['1', '0', s] = Card RT <$> parseSuit s -- Handle "10" rank explicitly
parseCard _      = Nothing

-- Parses a string into a Hand, sorting cards descending by rank
parseHand :: String -> Maybe Hand
parseHand str =
    case mapM parseCard (words str) of
        Just cards | length cards == 5 -> Just $ Hand (sortOn Down cards)
        _                              -> Nothing

-- Hand Evaluation

-- Helper to group cards by rank
groupRanks :: Hand -> [[Card]]
groupRanks = sortOn (Down . length) . groupBy (\c1 c2 -> rank c1 == rank c2) . getCards

-- Helper to get ranks sorted descending
sortedRanks :: Hand -> [Rank]
sortedRanks = map rank . getCards

-- Check for Flush
isFlush :: Hand -> Bool
isFlush hand = let cards = getCards hand
               in length cards == 5 && all (\c -> suit c == suit (head cards)) (tail cards) -- Added length check for safety

-- Check for Straight (handles Ace-low A-2-3-4-5)
isStraight :: Hand -> Maybe Rank -- Returns highest rank if straight, Nothing otherwise
isStraight hand =
    let ranks = sortedRanks hand
        isConsecutive = all (\(r1, r2) -> fromEnum r1 == fromEnum r2 + 1) $ zip ranks (tail ranks)
        isAceLowStraight = ranks == [RA, R5, R4, R3, R2]
    in if isConsecutive then Just (head ranks)
       else if isAceLowStraight then Just R5 -- Ace-low straight highest card is 5
       else Nothing

-- Evaluate the rank of a hand
evaluateHand :: Hand -> HandRank
evaluateHand hand =
    let ranksDesc = sortedRanks hand
        groups = groupRanks hand
        groupLengths = map length groups
        groupRanks' = map (rank . head) groups -- Ranks corresponding to groups
        flush = isFlush hand
        maybeStraightRank = isStraight hand

    in case (flush, maybeStraightRank) of
        -- Handle Ace-low Straight Flush specifically (A 2 3 4 5 of same suit)
        (True, Just R5) -> StraightFlush R5 -- Ace-low straight flush highest card is 5
        (True, Just highRank) -> StraightFlush highRank -- Regular Straight Flush
        _ -> case groupLengths of
                 [4, 1] -> FourOfAKind (groupRanks' !! 0) (groupRanks' !! 1) -- Four of a Kind
                 [3, 2] -> FullHouse (groupRanks' !! 0) (groupRanks' !! 1)   -- Full House
                 _ | flush -> Flush ranksDesc                                -- Flush
                 _ | Just highRank <- maybeStraightRank -> Straight highRank -- Straight
                 [3, 1, 1] -> ThreeOfAKind (groupRanks' !! 0) (sortOn Down $ drop 1 groupRanks') -- Three of a Kind
                 [2, 2, 1] -> let pairs = sortOn Down $ take 2 groupRanks'
                              in TwoPair (pairs !! 0) (pairs !! 1) (groupRanks' !! 2) -- Two Pair
                 [2, 1, 1, 1] -> OnePair (groupRanks' !! 0) (sortOn Down $ drop 1 groupRanks') -- One Pair
                 _ -> HighCard ranksDesc                                     -- High Card


-- Main function

bestHands :: [String] -> Maybe [String]
bestHands handStrings = do
    -- Use traverse which is equivalent to mapM for Maybe Applicative/Monad
    parsedHands <- traverse (\s -> (,) s <$> parseHand s) handStrings
    if null parsedHands && not (null handStrings)
      then Nothing -- This case indicates a parsing failure occurred within traverse
      else if null parsedHands -- Only true if handStrings was empty initially
           then Just []
           else let evaluatedHands = map (\(s, h) -> (s, evaluateHand h)) parsedHands
                    -- Find the maximum rank among all evaluated hands
                    maxRankValue = maximum $ map snd evaluatedHands
                    -- Filter hands that match the maximum rank
                    winningHands = filter (\(_, handRank) -> handRank == maxRankValue) evaluatedHands -- Renamed rank -> handRank
                in Just $ map fst winningHands
