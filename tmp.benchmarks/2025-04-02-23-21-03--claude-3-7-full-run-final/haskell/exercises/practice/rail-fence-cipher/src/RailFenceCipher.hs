module RailFenceCipher (encode, decode) where

encode :: Int -> String -> String
encode rails message = concat [row r | r <- [0..rails-1]]
  where
    period = 2 * (rails - 1)
    positions = cycle [0..rails-1] ++ cycle [rails-2,rails-3..1]
    indexed = zip positions [0..] `zip` message
    row r = [c | ((rail, _), c) <- indexed, rail == r]

decode :: Int -> String -> String
decode rails message = map snd $ sortBy (comparing snd) withPositions
  where
    period = 2 * (rails - 1)
    totalLength = length message
    
    -- Generate rail pattern indices
    railPattern = cycle $ [0..rails-1] ++ [rails-2,rails-3..1]
    positions = take totalLength $ zip railPattern [0..]
    
    -- Count characters per rail
    railCounts = [length $ filter ((== r) . fst) positions | r <- [0..rails-1]]
    
    -- Calculate starting indices for each rail
    railStarts = scanl (+) 0 railCounts
    
    -- Assign characters to their original positions
    railChars = splitAtPoints railStarts message
    
    -- Group positions by rail
    railPositions = [[(rail, pos) | (rail, pos) <- positions, rail == r] | r <- [0..rails-1]]
    
    -- Combine characters with their positions
    withPositions = concat $ zipWith zipCharsWithPositions railChars railPositions
    
    zipCharsWithPositions chars posns = zip chars (map snd posns)

-- Helper functions
comparing :: Ord b => (a -> b) -> a -> a -> Ordering
comparing f x y = compare (f x) (f y)

splitAtPoints :: [Int] -> [a] -> [[a]]
splitAtPoints points xs = zipWith take lengths $ map (flip drop xs) points
  where lengths = zipWith (-) (tail points ++ [length xs]) points

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = foldr (insertBy cmp) []

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ x [] = [x]
insertBy cmp x (y:ys)
  | cmp x y /= GT = x : y : ys
  | otherwise = y : insertBy cmp x ys
