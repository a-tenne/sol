module Parser.Parser where

import AST
import Control.Monad (void)
import Control.Monad.Extra (anyM)
import Parser.Op
import Text.Parsec (char, digit, lookAhead, many, space, string, many1)
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

num :: Parser Integer
num = do
  skipSpace
  n <- many1 digit
  return $ read n

ex1 :: Parser Ex1
ex1 = do
  skipSpace
  l <- ex2
  r <- ex1'
  return Ex1 {lhs1 = l, rhs1 = r}

ex1' :: Parser Ex1'
ex1' = do
  skipSpace
  check <- checkSingle "or"
  if not check
    then return NIL1
    else do
      op <- oper1
      l <- ex2
      r <- ex1'
      return Ex1' {op1 = op, lhs'1 = l, rhs'1 = r}

ex2 :: Parser Ex2
ex2 = do
  l <- ex3
  r <- ex2'
  return Ex2 {lhs2 = l, rhs2 = r}

ex2' :: Parser Ex2'
ex2' = do
  skipSpace
  check <- checkSingle "and"
  if not check
    then return NIL2
    else do
      op <- oper2
      l <- ex3
      r <- ex2'
      return Ex2' {op2 = op, lhs'2 = l, rhs'2 = r}

ex3 :: Parser Ex3
ex3 = do
  l <- ex4
  r <- ex3'
  return Ex3 {lhs3 = l, rhs3 = r}

ex3' :: Parser Ex3'
ex3' = do
  skipSpace
  check <- checkMulti ["<=", ">=", "<", ">", "~=", "=="]
  if not check
    then return NIL3
    else do
      op <- oper3
      l <- ex4
      r <- ex3'
      return Ex3' {op3 = op, lhs'3 = l, rhs'3 = r}

ex4 :: Parser Ex4
ex4 = do
  l <- ex5
  r <- ex4'
  return Ex4 {lhs4 = l, rhs4 = r}

ex4' :: Parser Ex4'
ex4' = do
  skipSpace
  check <- checkSingle "|"
  if not check
    then return NIL4
    else do
      op <- oper4
      l <- ex5
      r <- ex4'
      return Ex4' {op4 = op, lhs'4 = l, rhs'4 = r}

ex5 :: Parser Ex5
ex5 = do
  l <- ex6
  r <- ex5'
  return Ex5 {lhs5 = l, rhs5 = r}

ex5' :: Parser Ex5'
ex5' = do
  skipSpace
  check <- checkSingle "~"
  if not check
    then return NIL5
    else do
      op <- oper5
      l <- ex6
      r <- ex5'
      return Ex5' {op5 = op, lhs'5 = l, rhs'5 = r}

ex6 :: Parser Ex6
ex6 = do
  l <- ex7
  r <- ex6'
  return Ex6 {lhs6 = l, rhs6 = r}

ex6' :: Parser Ex6'
ex6' = do
  skipSpace
  check <- checkSingle "&"
  if not check
    then return NIL6
    else do
      op <- oper6
      l <- ex7
      r <- ex6'
      return Ex6' {op6 = op, lhs'6 = l, rhs'6 = r}

ex7 :: Parser Ex7
ex7 = do
  l <- ex8
  r <- ex7'
  return Ex7 {lhs7 = l, rhs7 = r}

ex7' :: Parser Ex7'
ex7' = do
  skipSpace
  check <- checkMulti ["<<", ">>"]
  if not check
    then return NIL7
    else do
      op <- oper7
      l <- ex8
      r <- ex7'
      return Ex7' {op7 = op, lhs'7 = l, rhs'7 = r}

ex8 :: Parser Ex8
ex8 = do
  l <- ex9
  r <- ex8'
  return Ex8 {lhs8 = l, rhs8 = r}

ex8' :: Parser Ex8'
ex8' = do
  skipSpace
  check <- checkSingle ".."
  if not check
    then return NIL8
    else do
      op <- oper8
      l <- ex9
      r <- ex8'
      return Ex8' {op8 = op, lhs'8 = l, rhs'8 = r}

ex9 :: Parser Ex9
ex9 = do
  l <- ex10
  r <- ex9'
  return Ex9 {lhs9 = l, rhs9 = r}

ex9' :: Parser Ex9'
ex9' = do
  skipSpace
  check <- checkMulti ["+", "-"]
  if not check
    then return NIL9
    else do
      op <- oper9
      l <- ex10
      r <- ex9'
      return Ex9' {op9 = op, lhs'9 = l, rhs'9 = r}

ex10 :: Parser Ex10
ex10 = do
  l <- ex11
  r <- ex10'
  return Ex10 {lhs10 = l, rhs10 = r}

ex10' :: Parser Ex10'
ex10' = do
  skipSpace
  check <- checkMulti ["*", "/", "//", "%"]
  if not check
    then return NIL10
    else do
      op <- oper10
      l <- ex11
      r <- ex10'
      return Ex10' {op10 = op, lhs'10 = l, rhs'10 = r}

ex11 :: Parser Ex11
ex11 = do
  skipSpace
  check <- checkMulti ["not ", "#", "-", "~"]
  if not check
    then EX12 <$> ex12
    else do
      op <- oper11
      r <- ex11
      return Ex11 {op11 = op, rhs11 = r}

ex12 :: Parser Ex12
ex12 = do
  skipSpace
  l <- num
  r <- ex12'
  return Ex12 {lhs12 = l, rhs12 = r}

ex12' :: Parser Ex12'
ex12' = do
  skipSpace
  check <- checkSingle "^"
  if not check
    then return NIL12
    else do
      op <- oper12
      l <- num
      r <- ex12'
      return Ex12' {op12 = op, lhs'12 = l, rhs'12 = r}
