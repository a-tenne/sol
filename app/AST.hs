module AST where

import Data.Int (Int64)
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

data Numeric = NumInt Int64 | NumDouble Double
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
showFieldList (x : xs) =
  show x ++ case showFieldList xs of
    "" -> ""
    y -> ", " ++ y

instance Show TableConstructor where
  show (TableConstructor fields) = "{" ++ showFieldList fields ++ "}"

type Name = String

data Args = ArgList ExprList | ArgTable TableConstructor | ArgString Expr
  deriving (Eq)

instance Show Args where
  show(ArgList x) = "(" ++ show x ++ ")"
  show(ArgTable x) = show x
  show(ArgString x) = show x

data FuncCall where
  FuncCall :: PrefixExpr -> FuncCall
  deriving (Eq)

instance Show FuncCall where
  show (FuncCall x) = show x

data PrefixExpr = PrefixName Name PrefixExpr' | PrefixSub Expr PrefixExpr'
  deriving (Eq)

instance Show PrefixExpr where
  show(PrefixName x y) = x ++ show y
  show(PrefixSub x y) = "(" ++ show x ++ ")" ++ show y

data PrefixExpr' = TableIndex Expr PrefixExpr' | DotIndex Name PrefixExpr' | CallArgs Args PrefixExpr' | MethodArgs Name Args PrefixExpr' | PrefixEmpty
  deriving (Eq)

instance Show PrefixExpr' where
  show(TableIndex x y) = "[" ++ show x ++ "]" ++ show y
  show(DotIndex x y) = "." ++ x ++ show y
  show(MethodArgs x y z) = ":" ++ x ++ show y ++ show z
  show(CallArgs x y) = show x ++ show y
  show PrefixEmpty = ""

data Var where
  Var :: PrefixExpr -> Var
  deriving (Eq)

instance Show Var where
  show (Var x) = show x
  
data VarList where
  VarList :: [Var] -> VarList
  deriving (Eq)

showVarList :: VarList -> String
showVarList (VarList []) = ""
showVarList (VarList (x : xs)) =
  show x ++ case showVarList (VarList xs) of
    "" -> ""
    y -> ", " ++ y

instance Show VarList where
  show = showVarList

data Chunk where
  Chunk :: Block -> Chunk
  deriving(Eq)

instance Show Chunk where
  show (Chunk x) = show x


data Block = Block StatList (Maybe RetStat)
  deriving (Eq)

instance Show Block where
  show (Block l (Just r)) = show l ++ "\n" ++ show r
  show (Block l Nothing) = show l

data RetStat where
  RetStat :: ExprList -> RetStat
  deriving (Eq)

instance Show RetStat where
  show(RetStat (ExprList x)) = "return" ++ (if null x then "" else " ") ++ show (ExprList x) ++ ";"

data Stat = Semic | Asgn VarList ExprList
  deriving (Eq)

instance Show Stat where
  show Semic = ";"
  show (Asgn l r) = show l ++ " = " ++ show r
  
data StatList where
  StatList :: [Stat] -> StatList
  deriving (Eq)

showStatList :: StatList -> String
showStatList (StatList []) = ""
showStatList (StatList (x : xs)) =
  show x ++ case showStatList (StatList xs) of
    "" -> ""
    y -> "\n" ++ y

instance Show StatList where
  show = showStatList

data VarArg = VarArg
  deriving (Eq)

data NameList where
  NameList :: [Name] -> NameList
  deriving (Eq)

showNameList :: NameList -> String
showNameList (NameList []) = ""
showNameList (NameList (x : xs)) =
  show x ++ case showNameList (NameList xs) of
    "" -> ""
    y -> ", " ++ y

instance Show NameList where
  show = showNameList

data ParamList = ParamList NameList (Maybe VarArg)
  deriving (Eq)

instance Show ParamList where
  show (ParamList x (Just _)) = "(" ++ show x ++ ", ...)\n"
  show (ParamList x Nothing) = "(" ++ show x ++ ")\n"

data FuncBody = FuncBody ParamList Block
  deriving (Eq)

instance Show FuncBody where
  show (FuncBody x y) = "function " ++ show x ++ show y ++ "\nend\n"

data Expr = BinExpr Expr BIN_OP Expr | UnaryExpr U_OP Expr | LiteralExpr Literal | PreExpr PrefixExpr | NIL | TRUE | FALSE | TRIPLE_DOT | TableExpr TableConstructor | FunctionDef FuncBody
  deriving (Eq)

instance Show Expr where
  show (BinExpr l op r) = "(" ++ show l ++ show op ++ show r ++ ")"
  show (UnaryExpr op r) = "(" ++ show op ++ show r ++ ")"
  show (LiteralExpr l) = show l
  show (PreExpr x) = show x
  show NIL = "nil"
  show TRUE = "true"
  show FALSE = "false"
  show TRIPLE_DOT = "..."
  show (TableExpr x) = show x
  show (FunctionDef x) = show x

data ExprList where
  ExprList :: [Expr] -> ExprList
  deriving (Eq)

showExprList :: ExprList -> String
showExprList (ExprList []) = ""
showExprList (ExprList (x : xs)) =
  show x ++ case showExprList (ExprList xs) of
    "" -> ""
    y -> ", " ++ y

instance Show ExprList where
  show = showExprList
