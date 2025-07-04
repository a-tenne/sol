-- | Small module that contains binary and unary operator parsers
module Parser.Op where

import AST
import Text.Parsec (choice, string)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec (try)
import Prelude hiding (EQ, GT, LT)

-- | Parses one of the input strings and returns one of type a, which is either a binary or a unary operator in this case.
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

-- | Parses the input string and returns a value of type a, which is either a binary or a unary operator in this case.
opSingle :: (Show a, Eq a) => String -> a -> Parser a
opSingle x y = string x >> return y

-- | Parses the `or` operator.
oper1 :: Parser BIN_OP
oper1 = opSingle "or" OR

-- | Parses the `and` operator.
oper2 :: Parser BIN_OP
oper2 = opSingle "and" AND

-- | Parses one of the following operators: `<=`, `>=`, `<`, `>`, `~=`, `==`.
oper3 :: Parser BIN_OP
oper3 = opMulti ["<=", ">=", "<", ">", "~=", "=="] [LE, GE, LT, GT, NE, EQ]

-- | Parses the `|` operator.
oper4 :: Parser BIN_OP
oper4 = opSingle "|" B_OR

-- | Parses the `~` operator.
oper5 :: Parser BIN_OP
oper5 = opSingle "~" B_XOR

-- | Parses the `&` operator.
oper6 :: Parser BIN_OP
oper6 = opSingle "&" B_AND

-- | Parses one of the following operators: `<<`, `>>`.
oper7 :: Parser BIN_OP
oper7 = opMulti ["<<", ">>"] [LSHIFT, RSHIFT]

-- | Parses the `..` operator.
oper8 :: Parser BIN_OP
oper8 = opSingle ".." CONCAT

-- | Parses one of the following operators: `+`, `-`.
oper9 :: Parser BIN_OP
oper9 = opMulti ["+", "-"] [PLUS, MINUS]

-- | Parses one of the following operators: `*`, `//`, `/`, `%`.
oper10 :: Parser BIN_OP
oper10 = opMulti ["*", "//", "/", "%"] [MULT, INT_DIV, DIV, MOD]

-- | Parses one of the following (unary) operators: `not`, `#`, `-`, `+`.
oper11 :: Parser U_OP
oper11 = opMulti ["not ", "#", "-", "~"] [U_NOT, U_LEN, U_MINUS, U_B_NOT]

-- | Parses the `^` operator.
oper12 :: Parser BIN_OP
oper12 = opSingle "^" EXP
