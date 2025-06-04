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

oper1 :: Parser BIN_OP
oper1 = opSingle "or" OR

oper2 :: Parser BIN_OP
oper2 = opSingle "and" AND

oper3 :: Parser BIN_OP
oper3 = opMulti ["<=", ">=", "<", ">", "~=", "=="] [LE, GE, LT, GT, NE, EQ]

oper4 :: Parser BIN_OP
oper4 = opSingle "|" B_OR

oper5 :: Parser BIN_OP
oper5 = opSingle "~" B_XOR

oper6 :: Parser BIN_OP
oper6 = opSingle "&" B_AND

oper7 :: Parser BIN_OP
oper7 = opMulti ["<<", ">>"] [LSHIFT, RSHIFT]

oper8 :: Parser BIN_OP
oper8 = opSingle ".." CONCAT

oper9 :: Parser BIN_OP
oper9 = opMulti ["+", "-"] [PLUS, MINUS]

oper10 :: Parser BIN_OP
oper10 = opMulti ["*", "//", "/", "%"] [MULT, INT_DIV, DIV, MOD]

oper11 :: Parser U_OP
oper11 = opMulti ["not ", "#", "-", "~"] [U_NOT, U_LEN, U_MINUS, U_B_NOT]

oper12 :: Parser BIN_OP
oper12 = opSingle "^" EXP
