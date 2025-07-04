-- | Small module that consists of many helper functions
module Parser.Helpers where

import Control.Monad (void, when)
import Control.Monad.Extra (anyM)
import Text.Parsec (Parsec, anyChar, char, getInput, lookAhead, many, manyTill, space, string, try, (<|>))
import Text.Parsec.String (Parser)


-- | Parser that checks if the next string matches the input string.
checkSingle :: String -> Parser Bool
checkSingle str = lookAhead (try $ string str >> return True) <|> return False

-- | Parser that checks if the next string matches any of the input strings.
checkMulti :: [String] -> Parser Bool
checkMulti = anyM checkSingle

-- | Parser that checks if the next char matches the input parameter.
checkChar :: Char -> Parser Bool
checkChar x = lookAhead (try $ char x >> return True) <|> return False

-- | Parser that checks and returns the next char without consuming.
peekChar :: Parsec String () Char
peekChar = lookAhead anyChar

-- | Parser that tries to consume a multi line comment, then a single line comment.
skipComment :: Parser ()
skipComment = do
  void $ string "--"
  try multiLineComment <|> void (manyTill anyChar (char '\n'))

-- | Parser that consumes a multi line comment.
multiLineComment :: Parser ()
multiLineComment = do
  void $ char '['
  levelStr <- many $ char '='
  void $ char '['
  void $ manyTill anyChar (string $ "]" ++ levelStr ++ "]")

-- | Parser that consumes comments.
skipComments :: Parser ()
skipComments = void $ many skipComment

-- | Parser that consumes whitespace.
skipSpace :: Parser ()
skipSpace = void $ many space

-- | Parser that consumes whitespace and comments.
skipJunk :: Parser ()
skipJunk = do
  before <- getInput
  try skipSpace
  try skipComments <|> return ()
  after <- getInput
  when (before /= after) skipJunk

-- | List of reserved keywords in the Lua language.
reservedKW :: [String]
reservedKW = ["and", "break", "do", "else", "elseif", "end", "false", "for", "function", "goto", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while"]
