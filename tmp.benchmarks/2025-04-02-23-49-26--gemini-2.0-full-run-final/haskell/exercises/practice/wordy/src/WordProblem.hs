module WordProblem (answer) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

-- Define a simple language for parsing
languageDef :: P.LanguageDef
languageDef = emptyDef
  { P.identStart = letter
  , P.identLetter = alphaNum
  , P.opStart = oneOf "+-*/"
  , P.opLetter = oneOf "+-*/"
  , P.reservedOpNames = ["+", "-", "*", "/"]
  , P.reservedNames = ["What", "is", "plus", "minus", "multiplied", "divided", "by"]
  }

-- Create a token parser based on the language definition
lexer :: P.TokenParser String
lexer = P.makeTokenParser languageDef

-- Parser for integers
integer :: Parser Integer
integer = P.integer lexer

-- Parser for operators
operator :: Parser (Integer -> Integer -> Integer)
operator =
      (P.reservedOp lexer "plus" >> return (+))
  <|> (P.reservedOp lexer "minus" >> return (-))
  <|> (P.reservedOp lexer "multiplied" >> P.reservedOp lexer "by" >> return (*))
  <|> (P.reservedOp lexer "divided" >> P.reservedOp lexer "by" >> return div)

-- Parser for the word problem
problem :: Parser Integer
problem = do
  P.reserved lexer "What"
  P.reserved lexer "is"
  n <- integer
  rest <- many $ do
    op <- operator
    num <- integer
    return (op, num)
  P.eof
  return $ foldl (\acc (op, num) -> op acc num) n rest

-- Main function to parse and evaluate the problem
answer :: String -> Maybe Integer
answer problemString =
  case parse problem "" problemString of
    Right result -> Just result
    Left _ -> Nothing
