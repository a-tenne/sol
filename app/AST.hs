module AST where

import Data.Int (Int64)
import Data.List (intercalate)
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

instance Show TableConstructor where
  show (TableConstructor fields) = "{" ++ intercalate ", " (map show fields) ++ "}"

type Name = String

data Args = ArgList ExprList | ArgTable TableConstructor | ArgString Expr
  deriving (Eq)

instance Show Args where
  show (ArgList x) = "(" ++ show x ++ ")"
  show (ArgTable x) = show x
  show (ArgString x) = show x

data FuncCall where
  FuncCall :: PrefixExpr -> FuncCall
  deriving (Eq)

instance Show FuncCall where
  show (FuncCall x) = show x

data PrefixExpr = PrefixName Name PrefixExpr' | PrefixSub Expr PrefixExpr'
  deriving (Eq)

instance Show PrefixExpr where
  show (PrefixName x y) = x ++ show y
  show (PrefixSub x y) = "(" ++ show x ++ ")" ++ show y

data PrefixExpr' = TableIndex Expr PrefixExpr' | DotIndex Name PrefixExpr' | CallArgs Args PrefixExpr' | MethodArgs Name Args PrefixExpr' | PrefixEmpty
  deriving (Eq)

instance Show PrefixExpr' where
  show (TableIndex x y) = "[" ++ show x ++ "]" ++ show y
  show (DotIndex x y) = "." ++ x ++ show y
  show (MethodArgs x y z) = ":" ++ x ++ show y ++ show z
  show (CallArgs x y) = show x ++ show y
  show PrefixEmpty = ""

data Var where
  Var :: PrefixExpr -> Var
  deriving (Eq)

instance Show Var where
  show (Var x) = show x

data VarList where
  VarList :: [Var] -> VarList
  deriving (Eq)

instance Show VarList where
  show (VarList x) = intercalate ", " (map show x)

data Chunk where
  Chunk :: Block -> Chunk
  deriving (Eq)

instance Show Chunk where
  show (Chunk x) = show x

data Block = Block StatList (Maybe RetStat)
  deriving (Eq)

instance Show Block where
  show (Block l (Just r)) = (let stats = show l in if stats == "" then "" else stats ++ "\n") ++ show r
  show (Block l Nothing) = show l

data RetStat where
  RetStat :: ExprList -> RetStat
  deriving (Eq)

instance Show RetStat where
  show (RetStat (ExprList x)) = "return" ++ (if null x then "" else " ") ++ show (ExprList x)

data ElseIf = ElseIf Expr Block
  deriving (Eq)

instance Show ElseIf where
  show (ElseIf x y) = "elseif " ++ show x ++ " then\n" ++ show y

data ElseIfList where
  ElseIfList :: [ElseIf] -> ElseIfList
  deriving (Eq)

instance Show ElseIfList where
  show (ElseIfList x) = intercalate "\n" (map show x)

data Else where
  Else :: Block -> Else
  deriving (Eq)

instance Show Else where
  show (Else x) = "else\n" ++ show x

data FuncName = FuncName Name [Name] (Maybe Name)
  deriving (Eq)

instance Show FuncName where
  show (FuncName n1 nl (Just n2)) = n1 ++ concatMap ("." ++) nl ++ ':' : n2
  show (FuncName n1 nl Nothing) = n1 ++ concatMap ("." ++) nl

data Attrib where
  Attrib :: Name -> Attrib
  deriving (Eq)

instance Show Attrib where
  show (Attrib n) = "<" ++ n ++ ">"

data AttrNameList where
  AttrNameList :: [(Name, Maybe Attrib)] -> AttrNameList
  deriving (Eq)

instance Show AttrNameList where
  show (AttrNameList x) = intercalate ", " $ map (\(a, b) -> a ++ maybe "" show b) x

data Stat = Semic | Asgn VarList ExprList | Label Name | Break | Goto Name | Do Block | WhileDo Expr Block | RepeatUntil Block Expr | FuncCallStat FuncCall | IfStat Expr Block ElseIfList (Maybe Else) | ForStat Name Expr Expr (Maybe Expr) Block | ForIn NameList ExprList Block | FuncDefStat FuncName FuncBody | LocalFuncStat Name FuncBody | LocalAsgn AttrNameList (Maybe ExprList)
  deriving (Eq)

instance Show Stat where
  show Semic = ";"
  show (Asgn l r) = show l ++ " = " ++ show r
  show (Label x) = "::" ++ x ++ "::"
  show Break = "break"
  show (Goto x) = "goto " ++ x
  show (Do x) = "do\n" ++ show x ++ "\nend"
  show (WhileDo x y) = "while " ++ show x ++ " do\n" ++ show y ++ "\nend"
  show (RepeatUntil x y) = "repeat\n" ++ show x ++ "\nuntil " ++ show y
  show (FuncCallStat x) = show x
  show (IfStat ex b (ElseIfList elif) (Just el)) = "if " ++ show ex ++ " then\n" ++ show b ++ (if null elif then "" else "\n") ++ show (ElseIfList elif) ++ "\n" ++ show el ++ "\nend"
  show (IfStat ex b (ElseIfList elif) Nothing) = "if " ++ show ex ++ " then\n" ++ show b ++ (if null elif then "" else "\n") ++ show (ElseIfList elif) ++ "\nend"
  show (ForStat n e1 e2 (Just e3) b) = "for " ++ n ++ " = " ++ show e1 ++ ", " ++ show e2 ++ ", " ++ show e3 ++ "do\n" ++ show b ++ "\nend"
  show (ForStat n e1 e2 Nothing b) = "for " ++ n ++ " = " ++ show e1 ++ ", " ++ show e2 ++ "do\n" ++ show b ++ "\nend"
  show (ForIn nl el b) = "for " ++ show nl ++ " in " ++ show el ++ " do\n" ++ show b ++ "\nend"
  show (FuncDefStat n b) = "function " ++ show n ++ show b
  show (LocalFuncStat n b) = "local function " ++ n ++ show b
  show (LocalAsgn x (Just y)) = "local " ++ show x ++ " = " ++ show y
  show (LocalAsgn x Nothing) = "local " ++ show x

data StatList where
  StatList :: [Stat] -> StatList
  deriving (Eq)

instance Show StatList where
  show (StatList x) = intercalate "\n" (map show x)

data VarArg = VarArg
  deriving (Eq)

data NameList where
  NameList :: [Name] -> NameList
  deriving (Eq)

instance Show NameList where
  show (NameList x) = intercalate ", " x

data ParamList = ParamList NameList (Maybe VarArg)
  deriving (Eq)

instance Show ParamList where
  show (ParamList x (Just _)) = "(" ++ show x ++ ", ...)\n"
  show (ParamList x Nothing) = "(" ++ show x ++ ")\n"

data FuncBody = FuncBody ParamList Block
  deriving (Eq)

instance Show FuncBody where
  show (FuncBody x y) = show x ++ show y ++ "\nend"

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
  show (FunctionDef x) = "function " ++ show x

data ExprList where
  ExprList :: [Expr] -> ExprList
  deriving (Eq)

instance Show ExprList where
  show (ExprList x) = intercalate ", " (map show x)

data AST where
  AST :: Chunk -> AST
  deriving (Eq)

instance Show AST where
  show (AST x) = show x
