module Poker (bestHands) where

import Data.List (sort, group, sortBy)
import Data.Maybe (mapMaybe)

-- | 'bestHands' picks the best hand(s) from a list of poker hands.
--
-- It returns 'Nothing' if the list is empty.
-- Otherwise, returns 'Just' a list of the hand(s) that have the top rank.

bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hs =
  let valid = [(h, ph) | (h, Just ph) <- zip hs (map parseHand hs)]
      scored = map (\(h, c) -> (h, scoreHand c)) valid
  in if null scored
       then Nothing
       else
         let bestVal = maximum (map snd scored)
             winners = [ h | (h, sc) <- scored, sc == bestVal ]
         in Just winners

------------------------------------------------------------------------------
-- Card parsing
------------------------------------------------------------------------------

-- We represent a card as (rankValue, suitChar).
-- rankValue is an Int from 2..14, suitChar is one of 'H', 'D', 'C', 'S'.

parseHand :: String -> Maybe [(Int, Char)]
parseHand str =
  let cards = words str
  in if length cards /= 5
       then Nothing
       else sequence (map parseCard cards)

parseCard :: String -> Maybe (Int, Char)
parseCard c
  | length c < 2 = Nothing
  | otherwise =
      let suit = last c
          rankStr = take (length c - 1) c
          rankVal = parseRank rankStr
      in if suit `elem` "HDSC" && rankVal /= 0
           then Just (rankVal, suit)
           else Nothing

parseRank :: String -> Int
parseRank "A"   = 14
parseRank "K"   = 13
parseRank "Q"   = 12
parseRank "J"   = 11
parseRank "10"  = 10
parseRank "T"   = 10
parseRank "9"   = 9
parseRank "8"   = 8
parseRank "7"   = 7
parseRank "6"   = 6
parseRank "5"   = 5
parseRank "4"   = 4
parseRank "3"   = 3
parseRank "2"   = 2
parseRank _     = 0

------------------------------------------------------------------------------
-- Score a hand
--
-- We'll produce (rankCategory, tieBreakers) so we can compare them.
-- Larger 'rankCategory' is better. Then we compare tieBreakers lexicographically.
------------------------------------------------------------------------------

scoreHand :: [(Int, Char)] -> (Int, [Int])
scoreHand cards =
  let ranks = sort (map fst cards)  -- ascending
      suits = map snd cards
      isFl = isFlush suits
      isSt = isStraight ranks
      grouped = groupRanks ranks
      (cat, tie) = classify grouped isFl isSt ranks
  in (cat, tie)

-- groupRanks returns a descending list of (count, rank), sorted first by high count, then by rank
groupRanks :: [Int] -> [(Int, Int)]
groupRanks rs =
  let gr = group (reverse rs)  -- group from highest to lowest
      counts = map (\g -> (length g, head g)) gr
      sortedGroups = sortBy (\(c1, r1) (c2, r2) ->
                              if c1 /= c2 then compare c2 c1 else compare r2 r1
                            ) counts
  in sortedGroups

isFlush :: [Char] -> Bool
isFlush suits = all (== head suits) (tail suits)

isStraight :: [Int] -> Bool
isStraight rs =
  -- We have them ascending. For a normal straight, we check consecutive ranks.
  let diffs = zipWith (-) (tail rs) rs
  in all (==1) diffs
     || isAceLowStraight rs

-- Ace can be "low" in A-2-3-4-5
-- In that case ranks = [2,3,4,5,14], so let's detect that pattern.
isAceLowStraight :: [Int] -> Bool
isAceLowStraight [2,3,4,5,14] = True
isAceLowStraight _            = False

-- classify determines the category and tie breakers from the grouped info
classify :: [(Int, Int)]      -- ^ descending (count, rank)
         -> Bool              -- ^ flush?
         -> Bool              -- ^ straight?
         -> [Int]             -- ^ ascending ranks
         -> (Int, [Int])
classify grp isFl isSt sortedRanks
  | isFl && isSt = (8, reverse sortedRanks)  -- Straight Flush (category 8)
  | not (null grp) && fst (head grp) == 4 = -- Four of a Kind (7)
       case grp of
         [(_, r4), (_, r1)] -> (7, [r4, r1])
         _                  -> (7, reverse sortedRanks)  -- fallback
  | length grp == 2 && not (null grp) && fst (head grp) == 3 =
       -- Full House (6)
       case grp of
         [(_, r3), (_, r2)] -> (6, [r3, r2])
         _                  -> (6, reverse sortedRanks)
  | isFl = (5, reverse sortedRanks)   -- Flush
  | isSt = (4, reverse sortedRanks)   -- Straight
  | not (null grp) && fst (head grp) == 3 =
       -- Three of a kind (3)
       case grp of
         [(_, r3), (_, k1), (_, k2)] -> (3, [r3, max k1 k2, min k1 k2])
         _                           -> (3, reverse sortedRanks)
  | length grp == 3 =
       -- Could be two pairs (2) if (2, rank1), (2, rank2), (1, leftover)
       case grp of
         [g1, g2, g3] ->
           if fst g1 == 2 && fst g2 == 2
             then let [(_, p1), (_, p2), (_, kicker)] = grp
                  in (2, [max p1 p2, min p1 p2, kicker])
             else let (2, rPair) = g1
                      (_, k1) = g2
                      (_, k2) = g3
                  in (1, [rPair, max k1 k2, min k1 k2])
         _ -> (0, reverse sortedRanks)
  | length grp == 4 && not (null grp) && fst (head grp) == 2 =
       -- Exactly one pair
       let (2, rp) = head grp
           others = map snd (tail grp)
           sortedK = reverse (sort others)
       in (1, rp : sortedK)
  | otherwise =
       -- High card
       (0, reverse sortedRanks)
