module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.List (isPrefixOf, tails, findIndex)
import Control.Applicative ((<|>))

data CharPos = CharPos { col :: Int, row :: Int } deriving (Eq, Show)
data WordPos = WordPos { start :: CharPos, end :: CharPos } deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = [(word, findWord word) | word <- wordList]
  where
    findWord word = foldr ((<|>) . searchInDirection word) Nothing directions

    directions = [searchLeftRight, searchRightLeft, searchTopBottom, searchBottomTop, searchDiagonal]

    searchInDirection word searchFunc = searchFunc grid word

    searchLeftRight g word = searchInLines g word (zip [0..] g)
    searchRightLeft g word = searchInLines g (reverse word) (zip [0..] g)
    searchTopBottom g word = searchInLines (transpose g) word (zip [0..] (transpose g))
    searchBottomTop g word = searchInLines (transpose g) (reverse word) (zip [0..] (transpose g))
    searchDiagonal g word = searchDiagonals g word

    searchInLines g word indexedLines = foldr ((<|>) . findInLine word) Nothing indexedLines
      where
        findInLine w (rowIndex, line) = case findSubstring line w of
          Just colIndex -> Just (WordPos (CharPos colIndex rowIndex) (CharPos (colIndex + length w - 1) rowIndex))
          Nothing -> Nothing

    findSubstring line word = findIndex (isPrefixOf word) (tails line)

    searchDiagonals g word = foldr ((<|>) . findInDiagonal word) Nothing diagonalLines
      where
        diagonalLines = getDiagonals g

    findInDiagonal word diagonal = case findSubstring diagonal word of
      Just index -> Just (WordPos (CharPos index rowIndex) (CharPos (index + length word - 1) (rowIndex + length word - 1)))
      Nothing -> Nothing

    getDiagonals g = [getDiagonal g x y | x <- [0..length g - 1], y <- [0..length (head g) - 1]]

    getDiagonal g x y = [g !! (x + i) !! (y + i) | i <- [0..min (length g - x - 1) (length (head g) - y - 1)]]

    transpose ([]:_) = []
    transpose x = (map head x) : transpose (map tail x)
