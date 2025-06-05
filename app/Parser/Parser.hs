module Parser.Parser where
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Text.Parsec (many, space, (<|>), string, lookAhead, char, manyTill, anyChar, try, skipMany, getInput)
import Control.Monad.Extra (anyM)
import Control.Monad (when)

skipSpace :: Parser ()
skipSpace = void $ many space

checkSingle :: String -> Parser Bool
checkSingle str = lookAhead (string str >> return True) <|> return False

checkMulti :: [String] -> Parser Bool
checkMulti = anyM checkSingle

checkChar :: Char -> Parser Bool
checkChar x = lookAhead (char x >> return True) <|> return False

skipComment :: Parser ()
skipComment = do
  void $ string "--"
  try multiLineComment <|> void (manyTill anyChar (char '\n'))

multiLineComment :: Parser()
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

