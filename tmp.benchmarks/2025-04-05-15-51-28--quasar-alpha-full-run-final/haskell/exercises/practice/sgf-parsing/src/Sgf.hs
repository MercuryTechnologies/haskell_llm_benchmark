module Sgf (parseSgf) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))
import Text.Parsec
import Text.Parsec.Text (Parser)

type SgfTree = Tree SgfNode
type SgfNode = Map Text [Text]

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf sgf = case parse sgfTreeParser "" sgf of
    Right tree -> Just tree
    Left _ -> Nothing

sgfTreeParser :: Parser SgfTree
sgfTreeParser = do
    _ <- char '('
    spaces
    tree <- sequenceParser
    spaces
    _ <- char ')'
    return tree

sequenceParser :: Parser SgfTree
sequenceParser = do
    nodes <- many1 nodeParser
    children <- many variationParser
    return $ buildTree nodes children

variationParser :: Parser SgfTree
variationParser = sgfTreeParser

nodeParser :: Parser SgfNode
nodeParser = do
    _ <- char ';'
    props <- many propertyParser
    return $ Map.fromListWith (++) props

propertyParser :: Parser (Text, [Text])
propertyParser = do
    ident <- many1 upper
    values <- many1 propertyValueParser
    return (T.pack ident, values)

propertyValueParser :: Parser Text
propertyValueParser = do
    _ <- char '['
    content <- many (noneOf "]")
    _ <- char ']'
    return (T.pack content)

buildTree :: [SgfNode] -> [SgfTree] -> SgfTree
buildTree [] _ = error "No nodes to build tree from"
buildTree (n:ns) children =
    Node n (buildForest ns children)

buildForest :: [SgfNode] -> [SgfTree] -> [SgfTree]
buildForest [] children = children
buildForest (n:ns) children = [Node n (buildForest ns children)]
