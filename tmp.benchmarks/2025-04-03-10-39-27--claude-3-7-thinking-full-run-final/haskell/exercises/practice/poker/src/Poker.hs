module Poker (bestHands) where

import Data.List (sort, sortBy, group)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Char (toUpper)

-- Main function to find the best hand(s)
bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands handStrs =
    case traverse parseHand handStrs of
        Nothing -> Nothing
        Just hands ->
            let bestScore = maximum $ map snd hands
                bestHands' = map fst $ filter ((== bestScore) . snd) hands
            in Just bestHands'

-- Parse a hand into (original string, score)
parseHand :: String -> Maybe (String, [Int])
parseHand str = do
    let cardStrs = words str
    cards <- traverse parseCard cardStrs
    if length cards == 5
        then Just (str, scoreHand cards)
        else Nothing

-- Parse a card into (rank, suit)
parseCard :: String -> Maybe (Int, Char)
parseCard [r, s] = (,) <$> parseRank r <*> parseSuit s
parseCard ['1', '0', s] = (,) <$> Just 10 <*> parseSuit s
parseCard _ = Nothing

-- Parse a rank to an Int (2-14)
parseRank :: Char -> Maybe Int
parseRank c = case toUpper c of
    '2' -> Just 2
    '3' -> Just 3
    '4' -> Just 4
    '5' -> Just 5
    '6' -> Just 6
    '7' -> Just 7
    '8' -> Just 8
    '9' -> Just 9
    'T' -> Just 10
    'J' -> Just 11
    'Q' -> Just 12
    'K' -> Just 13
    'A' -> Just 14
    _   -> Nothing

-- Parse a suit to a Char
parseSuit :: Char -> Maybe Char
parseSuit c = case toUpper c of
    'S' -> Just 'S'
    'H' -> Just 'H'
    'D' -> Just 'D'
    'C' -> Just 'C'
    _   -> Nothing

-- Score a hand for comparison
scoreHand :: [(Int, Char)] -> [Int]
scoreHand cards =
    let ranks = map fst cards
        suits = map snd cards
        sortedRanks = reverse $ sort ranks
        groups = group $ sort ranks
        groupLengths = reverse $ sort $ map length groups
        ranksByFreq = concat $ sortBy (comparing (\g -> (-length g, -head g))) groups
        flush = length (nub suits) == 1
        straight = isStraight ranks
    in case (flush, straight, groupLengths) of
        (True, True, _) -> [8, straightHigh ranks] -- Straight flush
        (_, _, [4,1]) -> [7] ++ ranksByFreq -- Four of a kind
        (_, _, [3,2]) -> [6] ++ ranksByFreq -- Full house
        (True, _, _) -> [5] ++ sortedRanks -- Flush
        (_, True, _) -> [4, straightHigh ranks] -- Straight
        (_, _, [3,1,1]) -> [3] ++ ranksByFreq -- Three of a kind
        (_, _, [2,2,1]) -> [2] ++ ranksByFreq -- Two pair
        (_, _, [2,1,1,1]) -> [1] ++ ranksByFreq -- One pair
        _ -> [0] ++ sortedRanks -- High card

-- Check if ranks form a straight
isStraight :: [Int] -> Bool
isStraight ranks =
    let uniqueRanks = nub ranks
        sortedUnique = sort uniqueRanks
    in length uniqueRanks == 5 && (
        (maximum sortedUnique - minimum sortedUnique == 4) || -- Normal straight
        (sortedUnique == [2,3,4,5,14]) -- Ace-low straight
       )

-- Get the high card of a straight
straightHigh :: [Int] -> Int
straightHigh ranks =
    if sort (nub ranks) == [2,3,4,5,14]
        then 5 -- Ace-low straight
        else maximum ranks

-- Remove duplicates
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)
