module Sgf (parseSgf) where

import Data.Map  (Map, fromListWith)
import Data.Text (Text, pack, unpack)
import Data.Tree (Tree(..))
import Text.Parsec
import Text.Parsec.Text

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

parseSgf :: Text -> Maybe SgfTree
parseSgf sgf = case parse sgfParser "" sgf of
  Left _ -> Nothing
  Right tree -> Just tree

sgfParser :: Parsec Text () SgfTree
sgfParser = do
  char '('
  char ';'
  node <- nodeParser
  children <- many (try variationParser <|> sequenceParser)
  char ')'
  return $ Node node children

nodeParser :: Parsec Text () SgfNode
nodeParser = do
  props <- many propParser
  return $ fromListWith (++) props

propParser :: Parsec Text () (Text, [Text])
propParser = do
  key <- propKeyParser
  values <- many1 (propValueParser key)
  return (key, values)

propKeyParser :: Parsec Text () Text
propKeyParser = do
  key <- many1 upper
  return $ pack key

propValueParser :: Text -> Parsec Text () Text
propValueParser _ = do
  char '['
  value <- manyTill anyChar (char ']')
  return $ pack value

variationParser :: Parsec Text () SgfTree
variationParser = do
  char '('
  char ';'
  node <- nodeParser
  children <- many (try variationParser <|> sequenceParser)
  char ')'
  return $ Node node children

sequenceParser :: Parsec Text () SgfTree
sequenceParser = do
  char ';'
  node <- nodeParser
  children <- many (try variationParser <|> sequenceParser)
  return $ Node node children
