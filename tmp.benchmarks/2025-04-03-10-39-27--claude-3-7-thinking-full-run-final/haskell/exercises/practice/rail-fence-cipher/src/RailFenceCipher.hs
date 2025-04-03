module RailFenceCipher (encode, decode) where

-- Helper function to determine which rail a character belongs to
getRail :: Int -> Int -> Int
getRail numRails pos = 
  let cycle = 2 * (numRails - 1)
      pos' = pos `mod` cycle
  in if pos' < numRails then pos' else cycle - pos'

-- Encode a string using the Rail Fence Cipher
encode :: Int -> String -> String
encode numRails str
  | numRails <= 1 || null str = str  -- Edge cases
  | otherwise = concat [rail r | r <- [0..(numRails-1)]]
  where
    -- Get all characters that belong to rail r
    rail r = [c | (i, c) <- zip [0..] str, getRail numRails i == r]

-- Decode a string using the Rail Fence Cipher
decode :: Int -> String -> String
decode numRails str
  | numRails <= 1 || null str = str  -- Edge cases
  | otherwise = 
      let 
        len = length str
        
        -- Calculate which rail each position belongs to
        railPositions = [(getRail numRails i, i) | i <- [0..(len-1)]]
        
        -- Sort by rail to determine reading order in encoded string
        sortedByRail = sortPairs railPositions
        
        -- Pair each position with its character from encoded string
        posCharPairs = zip (map snd sortedByRail) str
        
        -- Sort by position to get original order
        sortedByPos = sortByFst posCharPairs
      in
        map snd sortedByPos
  where
    -- Sort pairs (a,b) by first component, then by second
    sortPairs :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
    sortPairs [] = []
    sortPairs (x:xs) = sortPairs [y | y <- xs, fst y < fst x || (fst y == fst x && snd y < snd x)]
                     ++ [x]
                     ++ sortPairs [y | y <- xs, fst y > fst x || (fst y == fst x && snd y >= snd x)]
    
    -- Sort pairs by first component
    sortByFst :: Ord a => [(a, b)] -> [(a, b)]
    sortByFst [] = []
    sortByFst (x:xs) = sortByFst [y | y <- xs, fst y < fst x]
                     ++ [x]
                     ++ sortByFst [y | y <- xs, fst y >= fst x]
