module OCR (convert) where

import Data.List (transpose)
import Data.Maybe (fromMaybe)

-- | Convert an OCR input string into the recognized digits, or an error if the size is invalid.
--   Multiple lines of digits are separated by blank lines in the input and
--   are joined by commas in the output.
convert :: String -> String
convert input =
    let allLines = lines input
        groupsOf4 = splitByBlankLines allLines
        convertedLines = map convert4Lines groupsOf4
    in  concatWithCommas convertedLines

-- | Split the input into groups of four lines. If there is a blank line separating
--   groups, it is consumed here. If the lines are not a multiple of 4 for a given group,
--   throw an error (invalid size).
splitByBlankLines :: [String] -> [[String]]
splitByBlankLines [] = []
splitByBlankLines ls =
    let (chunk, rest) = splitAt 4 ls
    in  if length chunk < 4
        then error "Invalid input size: each group must contain 4 lines."
        else
            let (blankLines, next) = span null rest
            in  chunk : splitByBlankLines next

-- | Convert exactly 4 lines of text into a string of recognized digits.
convert4Lines :: [String] -> String
convert4Lines lines4 =
    -- First, ensure each of the 4 lines is the same width.
    let widths = map length lines4
        maxW = if null widths then 0 else head widths
    in  if any (/= maxW) widths
        then error "Invalid input size: lines differ in width."
        else
            if maxW `mod` 3 /= 0
                then error "Invalid input size: width must be multiple of 3."
                else
                    let nDigits = maxW `div` 3
                        digits = [ extractDigit lines4 dIndex | dIndex <- [0..(nDigits-1)] ]
                    in map recognize digits

-- | Extract the 3x4 pattern for the digit at index dIndex from the 4 lines of text.
extractDigit :: [String] -> Int -> [String]
extractDigit lines4 dIndex =
    let startCol = dIndex * 3
        endCol = startCol + 3
    in map (take3Cols startCol endCol) lines4

take3Cols :: Int -> Int -> String -> String
take3Cols s e row = drop s (take e row)

-- | Look up a 3x4 digit pattern in the dictionary.
recognize :: [String] -> Char
recognize bloc =
    let key = unlines bloc  -- join the digit pattern into a single multi-line string
    in fromMaybe '?' (lookup key digits)

-- | Dictionary of known digits 0-9, stored as 4 lines of width 3 each.
digits :: [(String, Char)]
digits =
    [ (unlines [" _ "
               ,"| |"
               ,"|_|"
               ,"   "]
      ,'0')
    , (unlines ["   "
               ,"  |"
               ,"  |"
               ,"   "]
      ,'1')
    , (unlines [" _ "
               ," _|"
               ,"|_ "
               ,"   "]
      ,'2')
    , (unlines [" _ "
               ," _|"
               ," _|"
               ,"   "]
      ,'3')
    , (unlines ["   "
               ,"|_|"
               ,"  |"
               ,"   "]
      ,'4')
    , (unlines [" _ "
               ,"|_ "
               ," _|"
               ,"   "]
      ,'5')
    , (unlines [" _ "
               ,"|_ "
               ,"|_|"
               ,"   "]
      ,'6')
    , (unlines [" _ "
               ,"  |"
               ,"  |"
               ,"   "]
      ,'7')
    , (unlines [" _ "
               ,"|_|"
               ,"|_|"
               ,"   "]
      ,'8')
    , (unlines [" _ "
               ,"|_|"
               ," _|"
               ,"   "]
      ,'9')
    ]

-- | Join a list of string digits with commas.
concatWithCommas :: [String] -> String
concatWithCommas = foldr1 joinWithComma
  where
    joinWithComma s acc = s ++ "," ++ acc
