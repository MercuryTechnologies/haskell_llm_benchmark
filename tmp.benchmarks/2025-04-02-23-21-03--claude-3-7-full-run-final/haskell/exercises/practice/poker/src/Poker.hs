module Poker (bestHands) where

import Data.List (sortBy, groupBy, sort, group)
import Data.Ord (comparing, Down(..))
import Data.Char (isDigit, digitToInt)
import Data.Function (on)

data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Show, Eq)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord, Enum)

data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Show, Eq)

data HandRank = HighCard [Rank]
              | OnePair Rank [Rank]
              | TwoPair Rank Rank Rank
              | ThreeOfAKind Rank [Rank]
              | Straight Rank
              | Flush [Rank]
              | FullHouse Rank Rank
              | FourOfAKind Rank Rank
              | StraightFlush Rank
  deriving (Show, Eq, Ord)

-- | Parse a card from a string like "4S" (4 of Spades)
parseCard :: String -> Card
parseCard [r, s] = Card (parseRank r) (parseSuit s)
parseCard ['1', '0', s] = Card Ten (parseSuit s)
parseCard _ = error "Invalid card format"

-- | Parse a rank from a character
parseRank :: Char -> Rank
parseRank c
  | c == 'A' = Ace
  | c == 'K' = King
  | c == 'Q' = Queen
  | c == 'J' = Jack
  | c == '1' = Ten  -- This is for "10"
  | isDigit c && digitToInt c >= 2 && digitToInt c <= 9 = toEnum (digitToInt c - 2)
  | otherwise = error $ "Invalid rank: " ++ [c]

-- | Parse a suit from a character
parseSuit :: Char -> Suit
parseSuit 'S' = Spades
parseSuit 'H' = Hearts
parseSuit 'D' = Diamonds
parseSuit 'C' = Clubs
parseSuit c = error $ "Invalid suit: " ++ [c]

-- | Parse a hand from a string like "4S 5S 7H 8D JC"
parseHand :: String -> [Card]
parseHand = map parseCard . words

-- | Evaluate a hand to determine its rank
evaluateHand :: [Card] -> HandRank
evaluateHand cards
  | isStraightFlush cards = StraightFlush (highestRank straightRanks)
  | isFourOfAKind cards = uncurry FourOfAKind (fourOfAKindRanks cards)
  | isFullHouse cards = uncurry FullHouse (fullHouseRanks cards)
  | isFlush cards = Flush (sortedRanks cards)
  | isStraight cards = Straight (highestRank straightRanks)
  | isThreeOfAKind cards = uncurry ThreeOfAKind (threeOfAKindRanks cards)
  | isTwoPair cards = uncurry3 TwoPair (twoPairRanks cards)
  | isOnePair cards = uncurry OnePair (onePairRanks cards)
  | otherwise = HighCard (sortedRanks cards)
  where
    straightRanks = map rank cards
    uncurry3 f (a, b, c) = f a b c

-- | Check if a hand is a straight flush
isStraightFlush :: [Card] -> Bool
isStraightFlush cards = isFlush cards && isStraight cards

-- | Check if a hand is four of a kind
isFourOfAKind :: [Card] -> Bool
isFourOfAKind cards = any ((== 4) . length) (groupByRank cards)

-- | Get the ranks for a four of a kind hand (four of a kind rank, kicker rank)
fourOfAKindRanks :: [Card] -> (Rank, Rank)
fourOfAKindRanks cards = 
  let grouped = groupByRank cards
      fourGroup = head $ filter ((== 4) . length) grouped
      fourRank = rank (head fourGroup)
      kickers = filter ((/= fourRank) . rank) cards
      kickerRank = rank (head kickers)
  in (fourRank, kickerRank)

-- | Check if a hand is a full house
isFullHouse :: [Card] -> Bool
isFullHouse cards =
  let grouped = groupByRank cards
      lengths = map length grouped
  in sort lengths == [2, 3]

-- | Get the ranks for a full house (three of a kind rank, pair rank)
fullHouseRanks :: [Card] -> (Rank, Rank)
fullHouseRanks cards =
  let grouped = groupByRank cards
      threeGroup = head $ filter ((== 3) . length) grouped
      twoGroup = head $ filter ((== 2) . length) grouped
  in (rank (head threeGroup), rank (head twoGroup))

-- | Check if a hand is a flush
isFlush :: [Card] -> Bool
isFlush cards = length (groupBy ((==) `on` suit) (sortBy (comparing suit) cards)) == 1

-- | Check if a hand is a straight
isStraight :: [Card] -> Bool
isStraight cards =
  let ranks = sort $ map rank cards
      -- Special case for A-5 straight (Ace counts as 1)
      isLowStraight = ranks == [Two, Three, Four, Five, Ace]
      -- Normal case
      isNormalStraight = all (uncurry consecutive) (zip ranks (tail ranks))
  in (length (nub ranks) == 5) && (isNormalStraight || isLowStraight)
  where
    consecutive r1 r2 = fromEnum r2 - fromEnum r1 == 1
    nub = map head . group

-- | Check if a hand is three of a kind
isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind cards =
  let grouped = groupByRank cards
      lengths = map length grouped
  in sort lengths == [1, 1, 3]

-- | Get the ranks for a three of a kind (three of a kind rank, kickers)
threeOfAKindRanks :: [Card] -> (Rank, [Rank])
threeOfAKindRanks cards =
  let grouped = groupByRank cards
      threeGroup = head $ filter ((== 3) . length) grouped
      threeRank = rank (head threeGroup)
      kickers = reverse $ sort $ map rank $ filter ((/= threeRank) . rank) cards
  in (threeRank, kickers)

-- | Check if a hand is two pair
isTwoPair :: [Card] -> Bool
isTwoPair cards =
  let grouped = groupByRank cards
      lengths = map length grouped
  in sort lengths == [1, 2, 2]

-- | Get the ranks for a two pair (high pair rank, low pair rank, kicker rank)
twoPairRanks :: [Card] -> (Rank, Rank, Rank)
twoPairRanks cards =
  let grouped = groupByRank cards
      pairs = filter ((== 2) . length) grouped
      pairRanks = reverse $ sort $ map (rank . head) pairs
      kickers = filter (\c -> rank c `notElem` pairRanks) cards
      kickerRank = rank (head kickers)
  in (head pairRanks, pairRanks !! 1, kickerRank)

-- | Check if a hand is one pair
isOnePair :: [Card] -> Bool
isOnePair cards =
  let grouped = groupByRank cards
      lengths = map length grouped
  in sort lengths == [1, 1, 1, 2]

-- | Get the ranks for a one pair (pair rank, kickers)
onePairRanks :: [Card] -> (Rank, [Rank])
onePairRanks cards =
  let grouped = groupByRank cards
      pairGroup = head $ filter ((== 2) . length) grouped
      pairRank = rank (head pairGroup)
      kickers = reverse $ sort $ map rank $ filter ((/= pairRank) . rank) cards
  in (pairRank, kickers)

-- | Group cards by rank
groupByRank :: [Card] -> [[Card]]
groupByRank = groupBy ((==) `on` rank) . sortBy (comparing rank)

-- | Get the sorted ranks of cards (highest first)
sortedRanks :: [Card] -> [Rank]
sortedRanks = reverse . sort . map rank

-- | Get the highest rank in a list
highestRank :: [Rank] -> Rank
highestRank = maximum

-- | Find the best hands from a list of poker hands
bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hands =
  let parsedHands = map parseHand hands
      evaluatedHands = map evaluateHand parsedHands
      handWithRank = zip hands evaluatedHands
      maxRank = maximum evaluatedHands
      bestHands' = map fst $ filter ((== maxRank) . snd) handWithRank
  in Just bestHands'
