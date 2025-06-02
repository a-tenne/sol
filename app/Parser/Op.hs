module Parser.Op where

import AST
import Text.Parsec (choice, string)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec (try)
import Prelude hiding (EQ, GT, LT)

opMulti :: (Show a, Eq a) => [String] -> [a] -> Parser a
opMulti strs opers = do
  if length strs /= length opers
    then
      error $ "Length of " ++ show strs ++ " has to match length of " ++ show opers
    else do
      let table = zip strs opers
      res <- choice (map (try . string) strs)
      case lookup res table of
        Just x -> return x
        Nothing -> error "This should never happen"

opSingle :: (Show a, Eq a) => String -> a -> Parser a
opSingle x y = string x >> return y

oper1 :: Parser OP1
oper1 = opSingle "or" OR

oper2 :: Parser OP2
oper2 = opSingle "and" AND

oper3 :: Parser OP3
oper3 = opMulti ["<=", ">=", "<", ">", "~=", "=="] [LE, GE, LT, GT, NE, EQ]

oper4 :: Parser OP4
oper4 = opSingle "|" B_OR

oper5 :: Parser OP5
oper5 = opSingle "~" B_XOR

oper6 :: Parser OP6
oper6 = opSingle "&" B_AND

oper7 :: Parser OP7
oper7 = opMulti ["<<", ">>"] [LSHIFT, RSHIFT]

oper8 :: Parser OP8
oper8 = opSingle ".." CONCAT

oper9 :: Parser OP9
oper9 = opMulti ["+", "-"] [PLUS, MINUS]

oper10 :: Parser OP10
oper10 = opMulti ["*", "//", "/", "%"] [MULT, INT_DIV, DIV, MOD]

oper11 :: Parser OP11
oper11 = opMulti ["not", "#", "-", "~"] [NOT, INDEX, U_MINUS, B_NOT]

oper12 :: Parser OP12
oper12 = opSingle "^" EXP
