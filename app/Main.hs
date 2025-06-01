module Main where

import System.Environment
import Text.Parsec (digit, oneOf, parse, skipMany, lookAhead, (<|>))
import Text.Parsec.Char (space)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (Parser)

num :: Parser Integer
num = do
  n <- many1 digit
  return (read n)


data Expr1
  = Binary1
      { lhs1 :: Integer,
        op1 :: Char,
        rhs1 :: Expr1
      }
  | Unary1 Integer
  deriving (Show)

data Expr2
  = Binary2
      { lhs2 :: Integer,
        op2 :: Char,
        rhs2 :: Expr2
      }
  | Unary2 Integer
  deriving (Show)

binary1 :: Parser Char
binary1 = oneOf "+-"

binary2 :: Parser Char
binary2 = oneOf "*/"

checkBinaryOp :: Parser Bool
checkBinaryOp = lookAhead (oneOf "+-*/" >> return True) <|> return False

expr1 :: Parser Expr1
expr1 = do
  whitespace
  l <- num
  whitespace
  check <- checkBinaryOp
  whitespace
  if not check then
    return (Unary1 l)
  else do
    op <- binary1
    r <- expr1
    whitespace
    return (Binary1 {lhs1 = l, op1 = op, rhs1 = r})

whitespace :: Parser ()
whitespace = skipMany space

main :: IO ()
main = do
  argv <- getArgs
  let fileName = last argv
  if null fileName
    then
      error "Input file required!"
    else do
      file <- readFile fileName
      case parse expr1 "Syntax error" file of
        Left err -> print err
        Right expr -> print expr
      return ()
