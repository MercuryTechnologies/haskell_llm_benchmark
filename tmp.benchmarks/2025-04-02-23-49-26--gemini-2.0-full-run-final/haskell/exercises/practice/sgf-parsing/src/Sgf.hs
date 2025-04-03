module Sgf (parseSgf) where

import Data.Char (isAlphaNum)
import Data.Map (Map, fromList, insert, empty)
import Data.Text (Text, pack, unpack)
import Data.Tree (Tree(Node))
import Text.Parsec (Parsec, parse, char, many1, many, (<|>), try, string, between, skipMany1, skipMany, eof)
import Text.Parsec.String (Parser)
import qualified Data.Text as T

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

parseSgf :: Text -> Maybe (Tree SgfNode)
parseSgf sgf =
  case parse sgfParser "" (unpack sgf) of
    Right tree -> Just tree
    Left _     -> Nothing

sgfParser :: Parser SgfTree
sgfParser = between (char '(') (char ')') nodeParser

nodeParser :: Parser SgfTree
nodeParser = do
  node <- propertiesParser
  children <- many sgfParser
  return (Node node children)

propertiesParser :: Parser SgfNode
propertiesParser = do
  skipMany skipWhiteSpace
  props <- many propertyParser
  return (fromList props)

propertyParser :: Parser (Text, [Text])
propertyParser = do
  char ';'
  key <- many1 (charToTextParser isAlphaNum)
  values <- many valueParser
  return (pack key, values)

valueParser :: Parser Text
valueParser = do
  between (char '[') (char ']') (many1 (charToTextParser (const True)) >>= return . pack)

charToTextParser :: (Char -> Bool) -> Parser Char
charToTextParser predicate = do
  c <- satisfyChar predicate
  return c

satisfyChar :: (Char -> Bool) -> Parser Char
satisfyChar predicate = do
  c <- anyChar
  if predicate c
    then return c
    else fail "predicate failed"

skipWhiteSpace :: Parser Char
skipWhiteSpace = char ' ' <|> char '\n' <|> char '\r' <|> char '\t'

anyChar :: Parser Char
anyChar = Text.Parsec.anyChar
