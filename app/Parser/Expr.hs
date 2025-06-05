module Parser.Expr where

import AST
import Control.Monad (void)
import Parser.Op
import Parser.Parser (checkMulti, checkSingle, skipJunk)
import Text.Parsec (anyChar, char, getInput, many, manyTill, oneOf, satisfy, string, try, (<|>))
import Text.Parsec.String (Parser)
import Parser.Nums
import Data.Functor (($>))

num :: Parser Expr
num = do
  n <- try numDouble <|> numInt
  check <- checkMulti [" ", "<=", ">=", "<", ">", "~=", "==", "|", "~", "&", "<<", ">>", "+", "-", "*", "/", "//", "%", "^", "\n", ")"]
  if not check
    then do
      input <- getInput
      if null input
        then
          return $ LiteralExpr (NumLit n)
        else fail "Malformed number"
    else return $ LiteralExpr (NumLit n)

singleLineStr :: Parser Expr
singleLineStr = do
  start <- oneOf "\"'"
  str <- manyTill (satisfy (/= '\n')) (char start)
  return $ LiteralExpr (StringLit str)

multiLineStr :: Parser Expr
multiLineStr =
    char '[' >> do
    levelStr <- many $ char '='
    void $ char '['
    str <- manyTill anyChar (string $ "]" ++ levelStr ++ "]")
    return $ LiteralExpr (StringLit str)

tripleDot :: Parser Expr
tripleDot = string "..." $> LiteralExpr TRIPLE_DOT

true :: Parser Expr
true = string "true" $> LiteralExpr TRUE

false :: Parser Expr
false = string "false" $> LiteralExpr FALSE

nil :: Parser Expr
nil = string "nil" $> LiteralExpr NIL


literalExpr :: Parser Expr
literalExpr = skipJunk >> try (num <|> singleLineStr <|> multiLineStr <|> true <|> false <|> tripleDot <|> nil)

ex1 :: Parser Expr
ex1 = skipJunk >> ex2 >>= ex1'

ex1' :: Expr -> Parser Expr
ex1' l = do
  skipJunk
  check <- checkSingle "or"
  if not check
    then return l
    else do
      op <- oper1
      r <- ex2
      ex1' $ BinExpr l op r

ex2 :: Parser Expr
ex2 = skipJunk >> ex3 >>= ex2'

ex2' :: Expr -> Parser Expr
ex2' l = do
  skipJunk
  check <- checkSingle "and"
  if not check
    then return l
    else do
      op <- oper2
      r <- ex3
      ex2' $ BinExpr l op r

ex3 :: Parser Expr
ex3 = skipJunk >> ex4 >>= ex3'

ex3' :: Expr -> Parser Expr
ex3' l = do
  skipJunk
  check <- checkMulti ["<=", ">=", "<", ">", "~=", "=="]
  if not check
    then return l
    else do
      op <- oper3
      r <- ex4
      ex3' $ BinExpr l op r

ex4 :: Parser Expr
ex4 = skipJunk >> ex5 >>= ex4'

ex4' :: Expr -> Parser Expr
ex4' l = do
  skipJunk
  check <- checkSingle "|"
  if not check
    then return l
    else do
      op <- oper4
      r <- ex5
      ex4' $ BinExpr l op r

ex5 :: Parser Expr
ex5 = skipJunk >> ex6 >>= ex5'

ex5' :: Expr -> Parser Expr
ex5' l = do
  skipJunk
  check <- checkSingle "~"
  if not check
    then return l
    else do
      op <- oper5
      r <- ex6
      ex5' $ BinExpr l op r

ex6 :: Parser Expr
ex6 = skipJunk >> ex7 >>= ex6'

ex6' :: Expr -> Parser Expr
ex6' l = do
  skipJunk
  check <- checkSingle "&"
  if not check
    then return l
    else do
      op <- oper6
      r <- ex7
      ex6' $ BinExpr l op r

ex7 :: Parser Expr
ex7 = skipJunk >> ex8 >>= ex7'

ex7' :: Expr -> Parser Expr
ex7' l = do
  skipJunk
  check <- checkMulti ["<<", ">>"]
  if not check
    then return l
    else do
      op <- oper7
      r <- ex8
      ex7' $ BinExpr l op r

ex8 :: Parser Expr
ex8 = skipJunk >> ex9 >>= ex8'

ex8' :: Expr -> Parser Expr
ex8' l = do
  skipJunk
  check <- checkSingle ".."
  if not check
    then return l
    else do
      op <- oper8
      r <- ex9
      ex8' $ BinExpr l op r

ex9 :: Parser Expr
ex9 = skipJunk >> ex10 >>= ex9'

ex9' :: Expr -> Parser Expr
ex9' l = do
  skipJunk
  check <- checkMulti ["+", "-"]
  if not check
    then return l
    else do
      op <- oper9
      r <- ex10
      ex9' $ BinExpr l op r

ex10 :: Parser Expr
ex10 = skipJunk >> ex11 >>= ex10'

ex10' :: Expr -> Parser Expr
ex10' l = do
  skipJunk
  check <- checkMulti ["*", "/", "//", "%"]
  if not check
    then return l
    else do
      op <- oper10
      r <- ex11
      ex10' $ BinExpr l op r

ex11 :: Parser Expr
ex11 = do
  skipJunk
  check <- checkMulti ["not ", "#", "-", "~"]
  if not check
    then ex12
    else UnaryExpr <$> oper11 <*> ex11

ex12 :: Parser Expr
ex12 = do
  skipJunk
  l <- try literalExpr <|> subEx
  ex12' l

ex12' :: Expr -> Parser Expr
ex12' l = do
  skipJunk
  check <- checkSingle "^"
  if not check
    then return l
    else do
      op <- oper12
      r <- literalExpr
      ex12' $ BinExpr l op r

subEx :: Parser Expr
subEx = do
  void $ char '('
  expr <- ex1
  void $ char ')'
  return expr
