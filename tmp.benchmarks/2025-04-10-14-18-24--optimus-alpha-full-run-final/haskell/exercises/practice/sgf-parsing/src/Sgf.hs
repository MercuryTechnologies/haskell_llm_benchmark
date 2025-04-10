module Sgf (parseSgf) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))
import Text.Parsec
import Text.Parsec.Text (Parser)
import Control.Applicative ((<|>), many)
import Data.Functor (($>))

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf input =
  case parse sgfTreeParser "" input of
    Right tree -> Just tree
    Left _ -> Nothing

-- Parsers

sgfTreeParser :: Parser SgfTree
sgfTreeParser = do
  spaces
  t <- tree
  spaces
  eof
  return t

tree :: Parser SgfTree
tree = do
  _ <- char '('
  nodes <- many1 node
  children <- many variation
  _ <- char ')'
  let (thisNode:restNodes) = nodes
      mainLine = foldl (\parent n -> Node n [parent]) (Node thisNode []) (reverse restNodes)
      allChildren = subForest mainLine ++ children
  return $ mainLine { subForest = allChildren }

variation :: Parser SgfTree
variation = tree

node :: Parser SgfNode
node = do
  _ <- char ';'
  props <- many property
  return $ Map.fromListWith (++) props

property :: Parser (Text, [Text])
property = do
  key <- many1 upper
  vals <- many1 propValue
  return (T.pack key, vals)

propValue :: Parser Text
propValue = do
  _ <- char '['
  val <- many (noneOf "]" <|> escapedChar)
  _ <- char ']'
  return $ T.pack val

escapedChar :: Parser Char
escapedChar = do
  _ <- char '\\'
  c <- anyChar
  return c
