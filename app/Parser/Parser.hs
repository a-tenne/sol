module Parser.Parser where

import AST
import Control.Monad (void)
import Control.Monad.Extra (anyM)
import Parser.Op
import Text.Parsec (char, digit, lookAhead, many, many1, space, string)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.String (Parser)

skipSpace :: Parser ()
skipSpace = void $ many space

checkSingle :: String -> Parser Bool
checkSingle str = lookAhead (string str >> return True) <|> return False

checkMulti :: [String] -> Parser Bool
checkMulti = anyM checkSingle

checkChar :: Char -> Parser Bool
checkChar x = lookAhead (char x >> return True) <|> return False

num :: Parser Expr
num = do
  skipSpace
  n <- many1 digit
  check <- checkMulti [" ", "<=", ">=", "<", ">", "~=", "==", "|", "~", "&", "<<", ">>", "+", "-", "*", "/", "//", "%", "^"]
  if not check then fail "Malformed number"
  else return (LiteralExpr (IntLit $ read n))

ex1 :: Parser Expr
ex1 = skipSpace >> ex2 >>= ex1'

ex1' :: Expr -> Parser Expr
ex1' l = do
  skipSpace
  check <- checkSingle "or"
  if not check
    then return l
    else do
      op <- oper1
      r <- ex2
      ex1' (BinExpr l op r)

ex2 :: Parser Expr
ex2 = skipSpace >> ex3 >>= ex2'

ex2' :: Expr -> Parser Expr
ex2' l = do
  skipSpace
  check <- checkSingle "and"
  if not check
    then return l
    else do
      op <- oper2
      r <- ex3
      ex2' (BinExpr l op r)

ex3 :: Parser Expr
ex3 = skipSpace >> ex4 >>= ex3'

ex3' :: Expr -> Parser Expr
ex3' l = do
  skipSpace
  check <- checkMulti ["<=", ">=", "<", ">", "~=", "=="]
  if not check
    then return l
    else do
      op <- oper3
      r <- ex4
      ex3' (BinExpr l op r)

ex4 :: Parser Expr
ex4 = skipSpace >> ex5 >>= ex4'

ex4' :: Expr -> Parser Expr
ex4' l = do
  skipSpace
  check <- checkSingle "|"
  if not check
    then return l
    else do
      op <- oper4
      r <- ex5
      ex4' (BinExpr l op r)

ex5 :: Parser Expr
ex5 = skipSpace >> ex6 >>= ex5'

ex5' :: Expr -> Parser Expr
ex5' l = do
  skipSpace
  check <- checkSingle "~"
  if not check
    then return l
    else do
      op <- oper5
      r <- ex6
      ex5' (BinExpr l op r)

ex6 :: Parser Expr
ex6 = skipSpace >> ex7 >>= ex6'

ex6' :: Expr -> Parser Expr
ex6' l = do
  skipSpace
  check <- checkSingle "&"
  if not check
    then return l
    else do
      op <- oper6
      r <- ex7
      ex6' (BinExpr l op r)

ex7 :: Parser Expr
ex7 = skipSpace >> ex8 >>= ex7'

ex7' :: Expr -> Parser Expr
ex7' l = do
  skipSpace
  check <- checkMulti ["<<", ">>"]
  if not check
    then return l
    else do
      op <- oper7
      r <- ex8
      ex7' (BinExpr l op r)

ex8 :: Parser Expr
ex8 = skipSpace >> ex9 >>= ex8'

ex8' :: Expr -> Parser Expr
ex8' l = do
  skipSpace
  check <- checkSingle ".."
  if not check
    then return l
    else do
      op <- oper8
      r <- ex9
      ex8' (BinExpr l op r)

ex9 :: Parser Expr
ex9 = skipSpace >> ex10 >>= ex9'

ex9' :: Expr -> Parser Expr
ex9' l = do
  skipSpace
  check <- checkMulti ["+", "-"]
  if not check
    then return l
    else do
      op <- oper9
      r <- ex10
      ex9' (BinExpr l op r)

ex10 :: Parser Expr
ex10 = skipSpace >> ex11 >>= ex10'

ex10' :: Expr -> Parser Expr
ex10' l = do
  skipSpace
  check <- checkMulti ["*", "/", "//", "%"]
  if not check
    then return l
    else do
      op <- oper10
      r <- ex11
      ex10' (BinExpr l op r)

ex11 :: Parser Expr
ex11 = do
  skipSpace
  check <- checkMulti ["not ", "#", "-", "~"]
  if not check
    then ex12
    else UnaryExpr <$> oper11 <*> ex11

ex12 :: Parser Expr
ex12 = skipSpace >> num >>= ex12'

ex12' :: Expr -> Parser Expr
ex12' l = do
  skipSpace
  check <- checkSingle "^"
  if not check
    then return l
    else do
      op <- oper12
      r <- num
      ex12' (BinExpr l op r)
