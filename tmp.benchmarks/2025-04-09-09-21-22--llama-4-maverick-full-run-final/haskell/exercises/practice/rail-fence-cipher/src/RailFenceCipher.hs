module RailFenceCipher (encode, decode) where

encode :: Int -> String -> String
encode _ "" = ""
encode numRails message = concat rails
  where
    rails = foldl addCharToRails (replicate numRails "") (zip message (cycle [0..numRails-1] ++ cycle [numRails-2,numRails-3..0]))
    addCharToRails rails (char, index) = take index rails ++ [char : rails !! index] ++ drop (index + 1) rails

decode :: Int -> String -> String
decode _ "" = ""
decode numRails ciphertext = readOffZigZag rails
  where
    railLengths = calculateRailLengths numRails (length ciphertext)
    rails = splitIntoRails ciphertext railLengths
    readOffZigZag :: [String] -> String
    readOffZigZag rails = concat $ snd $ foldl zigZag ([0, True], []) (replicate (length ciphertext) ())
    zigZag :: ([Int], [String]) -> () -> ([Int], [String])
    zigZag (state@(index, goingDown), acc) ()
      | goingDown = ((index + 1, index < numRails - 1), acc ++ [head (rails !! index)])
      | otherwise = ((index - 1, index > 0), acc ++ [head (rails !! index)])
    calculateRailLengths numRails len = map (`div` 2) $ take numRails $ scanl (+) 0 $ cycle ([2, 0] ++ replicate (numRails - 2) 1 ++ [0, 2])
    splitIntoRails :: String -> [Int] -> [String]
    splitIntoRails str lengths = map (take . fst) (zip lengths (iterate (drop . fst) (cycle [str])))
