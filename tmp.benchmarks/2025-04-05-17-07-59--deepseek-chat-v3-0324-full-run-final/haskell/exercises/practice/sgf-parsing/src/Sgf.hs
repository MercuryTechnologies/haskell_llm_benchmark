module Sgf (parseSgf) where

import Data.Map (Map, fromList)
import Data.Text (Text, pack)
import Data.Tree (Tree(Node))
import Text.Parsec
import Text.Parsec.Text
import Control.Monad (void)

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

parseSgf :: Text -> Maybe SgfTree
parseSgf input = case parse gameTree "" input of
    Right tree -> Just tree
    Left _ -> Nothing

gameTree :: Parser SgfTree
gameTree = do
    void $ char '('
    void $ char ';'
    node <- nodeContent
    children <- many (try gameTree)
    void $ char ')'
    return $ Node node children

nodeContent :: Parser SgfNode
nodeContent = do
    props <- many property
    return $ fromList props

property :: Parser (Text, [Text])
property = do
    key <- many1 upper
    values <- many1 value
    return (pack key, map pack values)

value :: Parser String
value = do
    void $ char '['
    content <- many (noneOf "]")
    void $ char ']'
    return content
