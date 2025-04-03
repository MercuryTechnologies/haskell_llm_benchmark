module Poker (bestHands) where

bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hands =
  let ranked = map (\h -> (h, rankHand h)) hands
      bestRank = maximum (map snd ranked)
      best = [h | (h, r) <- ranked, r == bestRank]
   in Just best

rankHand :: String -> Int
rankHand hand =
  let ranks = map (cardRank . head) (words hand)
  in maximum ranks

cardRank :: Char -> Int
cardRank 'A' = 14
cardRank 'K' = 13
cardRank 'Q' = 12
cardRank 'J' = 11
cardRank 'T' = 10
cardRank '9' = 9
cardRank '8' = 8
cardRank '7' = 7
cardRank '6' = 6
cardRank '5' = 5
cardRank '4' = 4
cardRank '3' = 3
cardRank '2' = 2
cardRank _   = 0
