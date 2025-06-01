module Main where

import System.Environment
import Text.Parsec (digit, skipMany)
import Text.Parsec.Char (space)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (Parser)

num :: Parser Integer
num = do
  n <- many1 digit
  return (read n)

whitespace :: Parser ()
whitespace = skipMany space

main :: IO ()
main = do
  argv <- getArgs
  let inFile = last argv
  if null inFile
    then
      error "Input file required!"
    else do
      fileHandle <- readFile inFile
      putStr fileHandle
      return ()
