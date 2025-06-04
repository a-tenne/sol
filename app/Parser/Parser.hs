module Parser.Parser where
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Text.Parsec (many, space, (<|>), string, lookAhead, char, manyTill, anyChar, try, skipMany)
import Control.Monad.Extra (anyM)

skipSpace :: Parser ()
skipSpace = void $ many space

checkSingle :: String -> Parser Bool
checkSingle str = lookAhead (string str >> return True) <|> return False

checkMulti :: [String] -> Parser Bool
checkMulti = anyM checkSingle

checkChar :: Char -> Parser Bool
checkChar x = lookAhead (char x >> return True) <|> return False

singleLineComment :: Parser ()
singleLineComment = do
  void $ string "--"
  void $ manyTill anyChar (char '\n')

multiLineComment :: Parser()
multiLineComment = do
  void $ char '['
  levelStr <- many $ char '='
  void $ char '['
  void $ manyTill anyChar (string $ "]" ++ levelStr ++ "]")


skipComment :: Parser ()
skipComment = skipSpace >> (try singleLineComment <|> try multiLineComment)

skipComments :: Parser ()
skipComments = skipMany skipComment
