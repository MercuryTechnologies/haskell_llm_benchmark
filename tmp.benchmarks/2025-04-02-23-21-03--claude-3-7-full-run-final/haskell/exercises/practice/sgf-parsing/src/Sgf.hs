module Sgf (parseSgf) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))
import Text.Parsec
import Text.Parsec.Text

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

-- | Parse an SGF string into a tree structure
parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf input = either (const Nothing) Just $ parse sgfParser "" input

-- | Main SGF parser
sgfParser :: Parser SgfTree
sgfParser = do
    void $ char '('
    void $ optional (char ';')
    tree <- gameTree
    void $ char ')'
    return tree

-- | Parse a game tree
gameTree :: Parser SgfTree
gameTree = do
    node <- nodeParser
    forest <- many $ try $ choice [
        between (char '(') (char ')') $ do
            void $ optional (char ';')
            gameTree
        , gameTree
        ]
    return $ Node node forest

-- | Parse a node
nodeParser :: Parser SgfNode
nodeParser = do
    props <- many propertyParser
    return $ Map.fromList props

-- | Parse a property
propertyParser :: Parser (Text, [Text])
propertyParser = do
    key <- propertyKey
    values <- many1 propertyValue
    return (key, values)

-- | Parse a property key
propertyKey :: Parser Text
propertyKey = do
    key <- many1 $ satisfy (\c -> c >= 'A' && c <= 'Z')
    return $ T.pack key

-- | Parse a property value
propertyValue :: Parser Text
propertyValue = between (char '[') (char ']') $ T.pack <$> many valueChar

-- | Parse a character in a property value
valueChar :: Parser Char
valueChar = escapedChar <|> normalChar
  where
    escapedChar = do
        void $ char '\\'
        oneOf "\\]"
    normalChar = noneOf "]"

-- | Skip whitespace
skipSpace :: Parser ()
skipSpace = void $ many $ satisfy isSpace
