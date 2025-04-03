module Sgf (parseSgf) where

import Data.Map  (Map, fromList)
import Data.Text (Text, pack)
import Data.Tree (Tree(Node))
import Text.Parsec
import Text.Parsec.Text

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

parseSgf :: Text -> Maybe SgfTree
parseSgf sgf = case parse sgfParser "" sgf of
    Left _  -> Nothing
    Right t -> Just t

sgfParser :: Parser SgfTree
sgfParser = do
    char '('
    nodes <- many1 nodeParser
    char ')'
    return $ Node (head nodes) (map Node (tail nodes))

nodeParser :: Parser SgfNode
nodeParser = do
    char ';'
    props <- many propertyParser
    return $ fromList props

propertyParser :: Parser (Text, [Text])
propertyParser = do
    key <- many1 upper
    values <- many1 (between (char '[') (char ']') (many (noneOf "]")))
    return (pack key, map pack values)
