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

data Literal = NumLit Numeric | StringLit String
  deriving (Eq)

instance Show Literal where
  show (NumLit x) = show x
  show (StringLit x) = show x

data Field = ExField Expr Expr | NamedField String Expr | SingleExField Expr
  deriving (Eq)

instance Show Field where
  show (ExField x y) = "[" ++ show x ++ "] = " ++ show y
  show (NamedField x y) = x ++ " = " ++ show y
  show (SingleExField x) = show x

data TableConstructor where
  TableConstructor :: [Field] -> TableConstructor
  deriving (Eq)

showFieldList :: [Field] -> String
showFieldList [] = ""
showFieldList (x:xs) = show x ++ case showFieldList xs of
  "" -> ""
  y -> ", " ++ y

instance Show TableConstructor where
  show (TableConstructor fields) = "{" ++ showFieldList fields ++ "}"

data Expr = BinExpr Expr BIN_OP Expr | UnaryExpr U_OP Expr | LiteralExpr Literal | VarExpr String | NIL | TRUE | FALSE | TRIPLE_DOT | TableExpr TableConstructor
  deriving (Eq)

instance Show Expr where
  show (BinExpr l op r) = "(" ++ show l ++ show op ++ show r ++ ")"
  show (UnaryExpr op r) = "(" ++ show op ++ show r ++ ")"
  show (LiteralExpr l) = show l
  show (VarExpr name) = '$' : name
  show NIL = "nil"
  show TRUE = "true"
  show FALSE = "false"
  show TRIPLE_DOT = "..."
  show (TableExpr x) = show x

data ExprList where
  ExprList :: [Expr] -> ExprList

showExprList :: ExprList -> String
showExprList (ExprList []) = ""
showExprList (ExprList (x:xs)) = show x ++ case showExprList (ExprList xs) of
  "" -> ""
  y -> ", " ++ y

instance Show ExprList where
  show = showExprList

{-
data Assign = Assign {asgnL :: NamedVar, asgnR :: Ex1}
  deriving (Eq)

instance Show Assign where
  show (Assign l r) = show l ++ " = " ++ show r

data Program = Program {ls :: [Assign]}
  deriving (Eq, Show)
  -}
