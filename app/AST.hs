module AST where

import Prelude hiding (EQ, GT, LT)

data BIN_OP = OR | AND | LE | GE | LT | GT | NE | EQ | B_OR | B_XOR | B_AND | LSHIFT | RSHIFT | CONCAT | PLUS | MINUS | MULT | DIV | INT_DIV | MOD | EXP
  deriving (Eq)

instance Show BIN_OP where
  show OR = " or "
  show AND = " and "
  show LT = " < "
  show GT = " > "
  show LE = " <= "
  show GE = " >= "
  show NE = " ~= "
  show EQ = " == "
  show B_OR = " | "
  show B_XOR = " ~ "
  show B_AND = " & "
  show LSHIFT = " << "
  show RSHIFT = " >> "
  show CONCAT = " .. "
  show PLUS = " + "
  show MINUS = " - "
  show MULT = " * "
  show DIV = " / "
  show INT_DIV = " // "
  show MOD = " % "
  show EXP = " ^ "


data U_OP = U_NOT | U_MINUS | U_LEN | U_B_NOT
  deriving (Eq)

instance Show U_OP where
  show U_NOT = "not "
  show U_MINUS = "-"
  show U_LEN = "#"
  show U_B_NOT = "~"

data Numeric = NumInt Integer | NumDouble Double
  deriving (Eq)

instance Show Numeric where
  show (NumInt x) = show x
  show (NumDouble x) = show x

data Literal = NumLit Numeric | StringLit String | NIL | TRUE | FALSE | TRIPLE_DOT
  deriving (Eq)

instance Show Literal where
  show (NumLit x) = show x
  show (StringLit x) = show x
  show NIL = show "nil"
  show TRUE = show "true"
  show FALSE = show "false"
  show TRIPLE_DOT = show "..."


data Expr = BinExpr  Expr BIN_OP Expr | UnaryExpr U_OP  Expr | LiteralExpr Literal
  deriving (Eq)

instance Show Expr where
  show (BinExpr l op r) = "(" ++ show l ++ show op ++ show r ++ ")"
  show (UnaryExpr op r) = "(" ++ show op ++ show r ++ ")"
  show (LiteralExpr l) = show l

{-
data NamedVar = NamedVar String

  deriving (Eq)

instance Show NamedVar where
  show (NamedVar name) = show name

data Assign = Assign {asgnL :: NamedVar, asgnR :: Ex1}
  deriving (Eq)

instance Show Assign where
  show (Assign l r) = show l ++ " = " ++ show r

data Program = Program {ls :: [Assign]}
  deriving (Eq, Show)
  -}
