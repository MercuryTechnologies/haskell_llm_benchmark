module Sgf (parseSgf) where

import Control.Applicative ((<|>), many, some)
import Control.Monad (void)
import Data.Map (Map, fromListWith)
import Data.Text (Text, pack)
import Data.Tree (Tree (..))
import Text.Parsec (ParseError, eof, many1, noneOf, parse, (<|>), try)
import Text.Parsec.Char (anyChar, char, letter, space, string, upper)
import Text.Parsec.Text (Parser) -- Use Parser defined for Text

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

-- | Main parsing function. Takes SGF Text and returns a Maybe SgfTree.
parseSgf :: Text -> Maybe SgfTree
parseSgf input = case parse sgfFile "(sgf)" input of
  Left _ -> Nothing
  Right result -> Just result

-- | Parser for the entire SGF file content, expecting exactly one tree.
sgfFile :: Parser SgfTree
sgfFile = sgfTree <* eof

-- | Parser for a single SGF tree structure: ( Sequence Variations )
sgfTree :: Parser SgfTree
sgfTree = do
  void $ char '('
  nodes <- sequenceNodes -- Parse the main sequence of nodes
  variations <- many sgfTree -- Parse any variations (subtrees)
  void $ char ')'
  return $ buildTree nodes variations

-- | Parser for a sequence of one or more nodes: ; Node ; Node ...
sequenceNodes :: Parser [SgfNode]
sequenceNodes = some (char ';' *> node) -- Must have at least one node after '('

-- | Helper function to construct the Tree structure from a list of nodes
-- | forming a path and a list of variations branching from the last node.
buildTree :: [SgfNode] -> [SgfTree] -> SgfTree
buildTree [] _ = error "buildTree: Empty node list is invalid SGF" -- Should be prevented by `some` in sequenceNodes
buildTree [n] variations = Node n variations -- Base case: last node holds the variations
buildTree (n : ns) variations = Node n [buildTree ns variations] -- Recursive case: build the path

-- | Parser for a single node, which consists of zero or more properties.
node :: Parser SgfNode
node = fromListWith (++) <$> many property -- Use fromListWith to merge values for the same key

-- | Parser for a single property: Key ValueList
-- | Example: FF[4] or AB[aa][ab]
property :: Parser (Text, [Text])
property = do
  key <- propIdent
  values <- some propValue -- A property must have at least one value
  return (key, values)

-- | Parser for a property identifier (Key): sequence of uppercase letters.
propIdent :: Parser Text
propIdent = pack <$> many1 upper

-- | Parser for a property value: [ Content ]
propValue :: Parser Text
propValue = do
  void $ char '['
  content <- pack <$> many valueChar -- Parse the content within brackets
  void $ char ']'
  return content
  where
    -- A character within a value can be escaped or normal.
    valueChar :: Parser Char
    valueChar = escaped <|> normal

    -- Escaped characters: '\' followed by any character. The escaped character is taken literally.
    escaped :: Parser Char
    escaped = char '\\' *> anyChar -- Consume '\', return the next char

    -- Normal characters: anything except ']' and '\'.
    normal :: Parser Char
    normal = noneOf "\\]"
