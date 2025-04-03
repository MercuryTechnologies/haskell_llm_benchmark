module Sgf (parseSgf) where

import Data.Map (Map, fromList)
import Data.Text (Text, pack)
import Data.Tree (Tree(Node))
import Text.Parsec (ParseError, Parsec, between, char, many, many1, noneOf, parse, try, (<|>))
import Text.Parsec.Char (letter)
import qualified Data.Text as T

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

-- | Parse an SGF string and return a tree of properties.
parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf sgf = case parse sgfParser "" (T.unpack sgf) of
    Left _ -> Nothing
    Right tree -> Just tree

-- | Parse an SGF tree.
sgfParser :: Parsec String () SgfTree
sgfParser = between (char '(') (char ')') gameTree

-- | Parse a game tree.
gameTree :: Parsec String () SgfTree
gameTree = do
    nodes <- many1 node
    forest <- many sgfParser
    return $ buildTree nodes forest
  where
    buildTree [n] f = Node n f
    buildTree (n:ns) _ = Node n [buildTree ns []]
    buildTree [] _ = error "Empty node list"

-- | Parse a node.
node :: Parsec String () SgfNode
node = do
    char ';'
    props <- many property
    return $ fromList props

-- | Parse a property.
property :: Parsec String () (Text, [Text])
property = do
    key <- many1 letter
    values <- many1 value
    return (pack key, values)

-- | Parse a property value.
value :: Parsec String () Text
value = between (char '[') (char ']') $ do
    val <- many (try escapedChar <|> normalChar)
    return $ pack val
  where
    normalChar = noneOf "\\]"
    escapedChar = char '\\' >> (char '\\' <|> char ']')
