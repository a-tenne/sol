module Parser.Helpers where

import Control.Monad (void, when)
import Control.Monad.Extra (anyM)
import Text.Parsec (anyChar, char, getInput, lookAhead, many, manyTill, space, string, try, (<|>), Parsec)
import Text.Parsec.String (Parser)

skipSpace :: Parser ()
skipSpace = void $ many space

checkSingle :: String -> Parser Bool
checkSingle str = lookAhead (try $ string str >> return True) <|> return False

checkMulti :: [String] -> Parser Bool
checkMulti = anyM checkSingle

checkChar :: Char -> Parser Bool
checkChar x = lookAhead (try $ char x >> return True) <|> return False

peekChar :: Parsec String () Char
peekChar = lookAhead anyChar

skipComment :: Parser ()
skipComment = do
  void $ string "--"
  try multiLineComment <|> void (manyTill anyChar (char '\n'))

multiLineComment :: Parser ()
multiLineComment = do
  void $ char '['
  levelStr <- many $ char '='
  void $ char '['
  void $ manyTill anyChar (string $ "]" ++ levelStr ++ "]")

skipComments :: Parser ()
skipComments = void $ many skipComment

skipJunk :: Parser ()
skipJunk = do
  before <- getInput
  skipSpace
  try skipComments <|> return ()
  after <- getInput
  when (before /= after) skipJunk

reservedKW :: [String]
reservedKW = ["and", "break", "do", "else", "elseif", "end", "false", "for", "function", "goto", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while"]
