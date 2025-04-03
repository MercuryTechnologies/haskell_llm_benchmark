module Poker (bestHands) where

import Data.List (group, sort, sortBy)
import Data.Maybe (mapMaybe)

-- A card has an integer rank (2..14 where 14 is Ace) and a suit (one of C, D, H, S)
data Card = Card { rank :: Int, suit :: Char } deriving (Show, Eq)

-- Parse a card from a string such as "2H" or "TD" or "AC" or "10D"
parseCard :: String -> Maybe Card
parseCard [r, s] = do
    rankVal <- parseRank [r]
    if s `elem` "CDHS" then return $ Card rankVal s else Nothing
parseCard ['1','0', s] = do
    rankVal <- parseRank "10"
    if s `elem` "CDHS" then return $ Card rankVal s else Nothing
parseCard _ = Nothing

parseRank :: String -> Maybe Int
parseRank [c]
  | c >= '2' && c <= '9' = Just (read [c])
  | c == 'T' = Just 10
  | c == 'J' = Just 11
  | c == 'Q' = Just 12
  | c == 'K' = Just 13
  | c == 'A' = Just 14
parseRank "10" = Just 10
parseRank _ = Nothing

-- Parse a hand from a string (five cards separated by whitespace)
parseHand :: String -> Maybe [Card]
parseHand str =
  let cardStrs = words str
  in if length cardStrs == 5 then mapM parseCard cardStrs else Nothing

-- Evaluate a hand and return a tuple indicating hand strength.
-- The first element (Int) is a category number (larger is better) and the list of Int gives tie-breakers.
evaluateHand :: [Card] -> (Int, [Int])
evaluateHand hand =
  let ranksDesc = sortBy (flip compare) $ map rank hand
      suits = map suit hand
      isFlush = all (== head suits) suits
      -- Check for straight in two variants: regular and Ace-low (A,5,4,3,2)
      isRegularStraight = and $ zipWith (\a b -> a - b == 1) ranksDesc (tail ranksDesc)
      isAceLowStraight = sort ranksDesc == [2,3,4,5,14]
      straightHigh = if isAceLowStraight then 5 else head ranksDesc
      isStraight = isRegularStraight || isAceLowStraight
      -- Group cards by rank frequency
      groups = sortBy (flip compareGroup) . map (\g -> (length g, head g)) . group . sort $ map rank hand
      compareGroup (count1, rank1) (count2, rank2) =
          compare count1 count2 <> compare rank1 rank2
  in case (isStraight, isFlush, groups) of
       (True, True, _)             -> (9, [straightHigh]) -- Straight Flush
       (_, _, ((4, fourRank):(1, kicker):[])) -> (8, [fourRank, kicker]) -- Four of a Kind
       (_, _, ((3, threeRank):(2, pairRank):[])) -> (7, [threeRank, pairRank]) -- Full House
       (_, True, _)                -> (6, ranksDesc) -- Flush
       (True, _, _)                -> (5, [straightHigh]) -- Straight
       (_, _, ((3, threeRank):rest)) -> (4, threeRank : kickers rest) -- Three of a Kind
       (_, _, ((2, pairRank1):(2, pairRank2):(1, kicker):[])) ->
           (3, sortDesc [pairRank1, pairRank2] ++ [kicker]) -- Two Pairs
       (_, _, ((2, pairRank):rest)) -> (2, pairRank : kickers rest) -- One Pair
       _                           -> (1, ranksDesc) -- High Card
  where
    kickers xs = sortDesc [r | (_, r) <- xs]
    sortDesc = sortBy (flip compare)

-- Compare two hands by evaluating them.
compareHands :: [Card] -> [Card] -> Ordering
compareHands h1 h2 =
  let ev1 = evaluateHand h1
      ev2 = evaluateHand h2
  in compare ev1 ev2

-- Pick the best hand(s) from a list of poker hand strings.
-- Returns Nothing for an empty input list, otherwise Just list of winning hand strings.
bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands handStrs =
  let parsed = map (\s -> (s, parseHand s)) handStrs
      validHands = mapMaybe (\(s, ph) -> fmap (\h -> (s, h)) ph) parsed
      evaluated = map (\(s, h) -> (s, evaluateHand h, h)) validHands
      bestScore = maximum [score | (_, score, _) <- evaluated]
      winners = [s | (s, score, _) <- evaluated, score == bestScore]
  in Just winners
