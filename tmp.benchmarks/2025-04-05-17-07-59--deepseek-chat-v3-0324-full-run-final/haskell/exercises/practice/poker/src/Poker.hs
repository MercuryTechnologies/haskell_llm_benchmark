module Poker (bestHands) where

import Data.List (sort, sortBy, group)
import Data.Ord (comparing)
import Data.Char (toUpper)

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show, Ord)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten 
          | Jack | Queen | King | Ace deriving (Eq, Show, Ord, Enum)
data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq, Show)

data HandRank = HighCard [Rank]
              | Pair Rank [Rank]
              | TwoPairs Rank Rank [Rank]
              | ThreeOfAKind Rank [Rank]
              | Straight Rank
              | Flush [Rank]
              | FullHouse Rank Rank
              | FourOfAKind Rank [Rank]
              | StraightFlush Rank
              deriving (Eq, Show, Ord)

parseCard :: String -> Card
parseCard [] = error "Empty card"
parseCard [r,s] = Card (parseRank r) (parseSuit s)
parseCard (r1:r2:s) = if r2 == '0'  -- handle '10'
                      then Card Ten (parseSuit (head s))
                      else error "Invalid card"

parseRank :: Char -> Rank
parseRank c = case toUpper c of
  '2' -> Two
  '3' -> Three
  '4' -> Four
  '5' -> Five
  '6' -> Six
  '7' -> Seven
  '8' -> Eight
  '9' -> Nine
  'T' -> Ten
  'J' -> Jack
  'Q' -> Queen
  'K' -> King
  'A' -> Ace
  _ -> error "Invalid rank"

parseSuit :: Char -> Suit
parseSuit c = case toUpper c of
  'C' -> Clubs
  'D' -> Diamonds
  'H' -> Hearts
  'S' -> Spades
  _ -> error "Invalid suit"

rankHand :: [Card] -> HandRank
rankHand cards = 
  let ranks = sortBy (flip compare) (map rank cards)
      suits = map suit cards
      groups = sortBy (comparing length) (group (sort ranks))
      isFlush = length (group suits) == 1
      isStraight = length (group ranks) == 5 && 
                   fromEnum (head ranks) - fromEnum (last ranks) == 4
  in case groups of
       [[a],[b],[c],[d],[e]] | isStraight && isFlush -> StraightFlush (head ranks)
       [[a,a,a,a],[b]]       -> FourOfAKind a [b]
       [[a,a,a],[b,b]]       -> FullHouse a b
       _ | isFlush           -> Flush ranks
       _ | isStraight        -> Straight (head ranks)
       [[a,a,a],[b],[c]]     -> ThreeOfAKind a (sortBy (flip compare) [b,c])
       [[a,a],[b,b],[c]]     -> TwoPairs a b [c]
       [[a,a],[b],[c],[d]]   -> Pair a (sortBy (flip compare) [b,c,d])
       _                     -> HighCard ranks

bestHands :: [String] -> Maybe [String]
bestHands hands = 
  let parsedHands = map (map parseCard . words) hands
      rankedHands = map (\h -> (h, rankHand h)) parsedHands
      maxRank = maximum (map snd rankedHands)
      bests = filter ((== maxRank) . snd) rankedHands
  in if null bests 
     then Nothing 
     else Just (map (unwords . map showCard . fst) bests)

showCard :: Card -> String
showCard (Card r s) = rankStr r ++ suitStr s
  where
    rankStr Ace = "A"
    rankStr King = "K"
    rankStr Queen = "Q"
    rankStr Jack = "J"
    rankStr Ten = "10"
    rankStr r = show (fromEnum r + 2)
    suitStr Clubs = "C"
    suitStr Diamonds = "D"
    suitStr Hearts = "H"
    suitStr Spades = "S"
