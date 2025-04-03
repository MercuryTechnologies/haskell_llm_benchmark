module Sgf (parseSgf) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))

import Text.Parsec
import Text.Parsec.String (Parser)

-- | parseSgf attempts to parse an SGF string into a tree of properties.
--   Returns Nothing on parse failure.
parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf input =
  case parse sgfTreeParser "" (T.unpack input) of
    Left _  -> Nothing
    Right t -> Just t

-- A GameTree in SGF is: "(" Sequence { GameTree } ")"
-- The Sequence is one or more Nodes in a row.
-- A Node is ";" followed by one or more properties.
-- A property is KEY[VALUE]+
-- KEY is one or more uppercase letters; each VALUE is bracketed.

sgfTreeParser :: Parser (Tree (Map Text [Text]))
sgfTreeParser = do
  char '('
  -- parse at least one node, i.e. a sequence of nodes
  firstSequence <- many1 parseNode
  -- zero or more subtrees (variations)
  subTrees <- many sgfTreeParser
  char ')'
  -- Link the sequence of nodes into a single chain of Tree
  let chained = chainNodes firstSequence
  -- The last node in that chain gets the subTrees as children
  return (attachSubtrees chained subTrees)

-- Convert [Map Text [Text]] into a linked chain of Tree (Map Text [Text]].
-- For example, if we have nodes [n1, n2, n3], then we create:
-- Node n1 [Node n2 [Node n3 []]]
chainNodes :: [Map Text [Text]] -> Tree (Map Text [Text])
chainNodes [] = error "chainNodes: unexpected empty list"
chainNodes (n:ns) = go n ns
  where
    go x []     = Node x []
    go x (y:ys) = Node x [go y ys]

-- Attach subTrees to the final node in the chain
attachSubtrees :: Tree (Map Text [Text]) -> [Tree (Map Text [Text])] -> Tree (Map Text [Text])
attachSubtrees (Node n []) subs = Node n subs
attachSubtrees (Node n [child]) subs = Node n [attachSubtrees child subs]
attachSubtrees (Node n cs) _ = Node n cs

parseNode :: Parser (Map Text [Text])
parseNode = do
  char ';'
  props <- many1 parseProperty
  -- each parseProperty returns (Text, [Text]) pairs
  -- if a key repeats in the same node, we append new values
  return (M.fromListWith (++) props)

parseProperty :: Parser (Text, [Text])
parseProperty = do
  key <- many1 upper
  vals <- many1 parseValue
  return (T.pack key, vals)

parseValue :: Parser Text
parseValue = do
  char '['
  -- We allow anything except ']' inside value. Escapes are not covered
  -- exhaustively here, but we keep it simple.
  content <- many (noneOf "]")
  char ']'
  return (T.pack content)
