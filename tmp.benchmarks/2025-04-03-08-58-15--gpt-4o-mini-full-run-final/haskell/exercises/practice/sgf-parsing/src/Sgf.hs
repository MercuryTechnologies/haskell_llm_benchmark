module Sgf (parseSgf) where

import Data.Map  (Map, empty, insertWith)
import Data.Text (Text, pack, unpack)
import Data.Tree (Tree(..))
import Text.Parsec
import Text.Parsec.Text

type SgfTree = Tree SgfNode
type SgfNode = Map Text [Text]

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf sgf = case parse sgfParser "" (unpack sgf) of
    Left _ -> Nothing
    Right tree -> Just tree

sgfParser :: Parser SgfTree
sgfParser = do
    char '('
    properties <- many propertyParser
    children <- many (try sgfParser)
    char ')'
    return $ Node (foldr insertProperties empty properties) children

propertyParser :: Parser (Text, [Text])
propertyParser = do
    key <- many1 upper
    values <- many valueParser
    return (pack key, values)

valueParser :: Parser Text
valueParser = do
    char '['
    value <- manyTill anyChar (char ']')
    return (pack value)

insertProperties :: (Text, [Text]) -> SgfNode -> SgfNode
insertProperties (key, values) = insertWith (++) key values
