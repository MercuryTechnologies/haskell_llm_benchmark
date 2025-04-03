module Sgf (parseSgf) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Tree (Tree(Node))
import Text.Parsec
import Text.Parsec.String (Parser)

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf sgf = case parse sgfGame "" (T.unpack sgf) of
    Left _  -> Nothing
    Right t -> Just t

-- Parser for an SGF game tree.
-- An SGF game tree is enclosed in parentheses.
-- Inside the parentheses there is a non-empty sequence of nodes,
-- followed by zero or more variations (subtrees) that branch from the last node.
sgfGame :: Parser (Tree (Map Text [Text]))
sgfGame = do
    spaces
    char '('
    nodes <- many1 sgfNode
    variations <- many sgfGame
    char ')'
    let mainBranch = buildChain nodes
        fullBranch = addVariations mainBranch variations
    return fullBranch

-- Build a chain tree from a non-empty list of nodes.
-- The chain is constructed by nesting nodes so that the first node is the parent of the second, 
-- the second the parent of the third, and so on.
buildChain :: [Map Text [Text]] -> Tree (Map Text [Text])
buildChain [n]    = Node n []
buildChain (n:ns) = Node n [buildChain ns]
buildChain []     = error "buildChain: empty list"

-- Add variations to the deepest node of the main branch.
-- The variations represent alternative continuations that branch from the last node.
addVariations :: Tree a -> [Tree a] -> Tree a
addVariations (Node a []) variations = Node a variations
addVariations (Node a (child:xs)) variations = Node a (addVariations child variations : xs)

-- Parser for an SGF node.
-- Each node starts with a semicolon ';' followed by one or more properties.
sgfNode :: Parser (Map Text [Text])
sgfNode = do
    char ';'
    props <- many sgfProperty
    return (Map.fromList props)

-- Parser for a property.
-- A property is a key (one or more letters) followed by one or more property values.
sgfProperty :: Parser (Text, [Text])
sgfProperty = do
    key <- many1 letter
    values <- many1 propertyValue
    return (T.pack key, values)

-- Parser for a property value.
-- Property values are enclosed in square brackets '[' and ']'.
propertyValue :: Parser Text
propertyValue = do
    char '['
    content <- many propertyChar
    char ']'
    return (T.pack content)

-- Parser for a character inside a property value.
-- Handles escaping: a backslash '\' escapes the next character.
propertyChar :: Parser Char
propertyChar = escapedChar <|> noneOf "]"

-- Parser for an escaped character.
escapedChar :: Parser Char
escapedChar = do
    char '\\'
    anyChar
