module Sgf (parseSgf) where

import Data.Map  (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree (..))

import Text.Parsec
import Text.Parsec.Text (Parser)

-- | Parse an SGF string and return a tree structure of properties.
-- Each node is a map from property key to list of property values.
parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf sgf =
  case parse pGameTree "" sgf of
    Left _  -> Nothing
    Right t -> Just t

-- | Parse a full game tree: '(' sequence { gametree } ')'
pGameTree :: Parser (Tree (Map Text [Text]))
pGameTree = between (char '(') (char ')') $ do
  nodes <- pSequence
  subs  <- many pGameTree
  return $ chainNodes nodes subs

-- | Parse a sequence of one or more nodes in a row.
pSequence :: Parser [Map Text [Text]]
pSequence = many1 pNode

-- | Parse a single node: ';' followed by zero or more properties.
pNode :: Parser (Map Text [Text])
pNode = do
  _     <- char ';'
  props <- many pProperty
  return $ foldl insertProp M.empty props
  where
    insertProp acc (k, vs) = M.insertWith (++) k vs acc

-- | Parse one property: property identifier followed by one or more values.
pProperty :: Parser (Text, [Text])
pProperty = do
  key  <- pPropIdent
  vals <- many1 pValue
  return (key, vals)

-- | Parse a property identifier: one or more uppercase letters.
pPropIdent :: Parser Text
pPropIdent = T.pack <$> many1 (oneOf ['A'..'Z'])

-- | Parse a property value: '[' ... ']'
-- The contents can include escaped brackets like '\]'.
pValue :: Parser Text
pValue = between (char '[') (char ']') pCValueType

-- | Parse the contents of a property value, handling escape characters.
pCValueType :: Parser Text
pCValueType = T.pack <$> many (escapedChar <|> normalChar)
  where
    escapedChar = char '\\' >> anyChar
    normalChar  = noneOf "]"

-- | Helper to chain a list of nodes into a linear path,
-- attaching any subtrees to the last node.
chainNodes :: [Map Text [Text]]
           -> [Tree (Map Text [Text])]
           -> Tree (Map Text [Text])
chainNodes []     _    = error "Unexpected empty sequence of nodes."
chainNodes [n]    subs = Node n subs
chainNodes (n:ns) subs = Node n [chainNodes ns subs]
