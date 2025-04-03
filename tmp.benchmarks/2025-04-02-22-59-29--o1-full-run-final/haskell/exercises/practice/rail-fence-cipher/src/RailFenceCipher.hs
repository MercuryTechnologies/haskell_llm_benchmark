module RailFenceCipher (encode, decode) where

import Data.List (sortOn)

encode :: Int -> String -> String
encode rails s =
    let -- remove spaces to match the examples
        s'       = filter (/= ' ') s
        -- make a cycle of row indices going down then up
        waveCycle = [0..rails-1] ++ [rails-2, rails-3..1]
        -- pair each character with a row index
        wave      = zip (take (length s') (cycle waveCycle)) s'
    in  -- sort by row index and pick out the characters
        map snd (sortOn fst wave)

decode :: Int -> String -> String
decode rails cipher =
    let len         = length cipher
        -- repeating pattern of row indices, length of the cipher
        wavePattern = take len (cycle ([0..rails-1] ++ [rails-2, rails-3..1]))
        -- how many chars go to each row
        rowCounts   = [ length (filter (== r) wavePattern) | r <- [0..rails-1] ]
        -- split the cipher among rows
        rowSlices   = splitBy rowCounts cipher
        -- rebuild the original order using the wave pattern
    in  rebuild wavePattern rowSlices

-- split a string into pieces according to a list of lengths
splitBy :: [Int] -> String -> [String]
splitBy [] _ = []
splitBy (n:ns) s =
    let (prefix, rest) = splitAt n s
    in  prefix : splitBy ns rest

-- rebuild the message by zipping characters from each row slice according to the wave pattern
rebuild :: [Int] -> [String] -> String
rebuild wavePattern rowSlices =
    let -- keep track of how far we've read in each row
        go [] _ _ = []
        go (r:rs) slices idxs =
            let rowIndex   = idxs !! r
                currentRow = slices !! r
                c          = currentRow !! rowIndex
                newIdxs    = replaceAt r (rowIndex + 1) idxs
            in  c : go rs slices newIdxs
    in  go wavePattern rowSlices (replicate (length rowSlices) 0)

-- replaceAt index value list
replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs
