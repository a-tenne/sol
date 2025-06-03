module AST where

import Prelude hiding (EQ, GT, LT)

data OP1 = OR
  deriving (Eq)

instance Show OP1 where
  show OR = " or "

data OP2 = AND
  deriving (Eq)

instance Show OP2 where
  show AND = " and "

data OP3 = LT | GT | LE | GE | NE | EQ
  deriving (Eq)

instance Show OP3 where
  show LT = " < "
  show GT = " > "
  show LE = " <= "
  show GE = " >= "
  show NE = " ~= "
  show EQ = " == "

data OP4 = B_OR
  deriving (Eq)

instance Show OP4 where
  show B_OR = " | "

data OP5 = B_XOR
  deriving (Eq)

instance Show OP5 where
  show B_XOR = " ~ "

data OP6 = B_AND
  deriving (Eq)

instance Show OP6 where
  show B_AND = " & "

data OP7 = LSHIFT | RSHIFT
  deriving (Eq)

instance Show OP7 where
  show LSHIFT = " << "
  show RSHIFT = " >> "

data OP8 = CONCAT
  deriving (Eq)

instance Show OP8 where
  show CONCAT = " .. "

data OP9 = PLUS | MINUS
  deriving (Eq)

instance Show OP9 where
  show PLUS = " + "
  show MINUS = " - "

data OP10 = MULT | DIV | INT_DIV | MOD
  deriving (Eq)

instance Show OP10 where
  show MULT = " * "
  show DIV = " / "
  show INT_DIV = " // "
  show MOD = " % "

data OP11 = NOT | INDEX | U_MINUS | B_NOT
  deriving (Eq)

instance Show OP11 where
  show NOT = "not "
  show INDEX = "#"
  show U_MINUS = "-"
  show B_NOT = "~"

data OP12 = EXP
  deriving (Eq)

instance Show OP12 where
  show EXP = " ^ "

data Ex1
  = Ex1
  { lhs1 :: Ex2,
    rhs1 :: Ex1'
  }
  deriving (Eq)

data Ex1' = Ex1' {op1 :: OP1, lhs'1 :: Ex2, rhs'1 :: Ex1'} | NIL1
  deriving (Eq)

instance Show Ex1 where
  show (Ex1 l r) = show l ++ show r

instance Show Ex1' where
  show (Ex1' op l r) = show op ++ show l ++ show r
  show NIL1 = ""

data Ex2
  = Ex2
  { lhs2 :: Ex3,
    rhs2 :: Ex2'
  }
  deriving (Eq)

data Ex2' = Ex2' {op2 :: OP2, lhs'2 :: Ex3, rhs'2 :: Ex2'} | NIL2
  deriving (Eq)

instance Show Ex2 where
  show (Ex2 l r) = show l ++ show r

instance Show Ex2' where
  show (Ex2' op l r) = show op ++ show l ++ show r
  show NIL2 = ""

data Ex3
  = Ex3
  { lhs3 :: Ex4,
    rhs3 :: Ex3'
  }
  deriving (Eq)

data Ex3' = Ex3' {op3 :: OP3, lhs'3 :: Ex4, rhs'3 :: Ex3'} | NIL3
  deriving (Eq)

instance Show Ex3 where
  show (Ex3 l r) = show l ++ show r

instance Show Ex3' where
  show (Ex3' op l r) = show op ++ show l ++ show r 
  show NIL3 = ""

data Ex4
  = Ex4
  { lhs4 :: Ex5,
    rhs4 :: Ex4'
  }
  deriving (Eq)

data Ex4' = Ex4' {op4 :: OP4, lhs'4 :: Ex5, rhs'4 :: Ex4'} | NIL4
  deriving (Eq)

instance Show Ex4 where
  show (Ex4 l r) = show l ++ show r

instance Show Ex4' where
  show (Ex4' op l r) = show op ++ show l ++ show r
  show NIL4 = ""

data Ex5
  = Ex5
  { lhs5 :: Ex6,
    rhs5 :: Ex5'
  }
  deriving (Eq)

data Ex5' = Ex5' {op5 :: OP5, lhs'5 :: Ex6, rhs'5 :: Ex5'} | NIL5
  deriving (Eq)

instance Show Ex5 where
  show (Ex5 l r) = show l ++ show r

instance Show Ex5' where
  show (Ex5' op l r) = show op ++ show l ++ show r
  show NIL5 = ""

data Ex6
  = Ex6
  { lhs6 :: Ex7,
    rhs6 :: Ex6'
  }
  deriving (Eq)

data Ex6' = Ex6' {op6 :: OP6, lhs'6 :: Ex7, rhs'6 :: Ex6'} | NIL6
  deriving (Eq)

instance Show Ex6 where
  show (Ex6 l r) = show l ++ show r

instance Show Ex6' where
  show (Ex6' op l r) = show op ++ show l ++ show r  
  show NIL6 = ""

data Ex7
  = Ex7
  { lhs7 :: Ex8,
    rhs7 :: Ex7'
  }
  deriving (Eq)

data Ex7' = Ex7' {op7 :: OP7, lhs'7 :: Ex8, rhs'7 :: Ex7'} | NIL7
  deriving (Eq)

instance Show Ex7 where
  show (Ex7 l r) = show l ++ show r

instance Show Ex7' where
  show (Ex7' op l r) = show op ++ show l ++ show r 
  show NIL7 = ""

data Ex8
  = Ex8
  { lhs8 :: Ex9,
    rhs8 :: Ex8'
  }
  deriving (Eq)

data Ex8' = Ex8' {op8 :: OP8, lhs'8 :: Ex9, rhs'8 :: Ex8'} | NIL8
  deriving (Eq)

instance Show Ex8 where
  show (Ex8 l r) = show l ++ show r

instance Show Ex8' where
  show (Ex8' op l r) = show op ++ show l ++ show r
  show NIL8 = ""

data Ex9
  = Ex9
  { lhs9 :: Ex10,
    rhs9 :: Ex9'
  }
  deriving (Eq)

data Ex9' = Ex9' {op9 :: OP9, lhs'9 :: Ex10, rhs'9 :: Ex9'} | NIL9
  deriving (Eq)

instance Show Ex9 where
  show (Ex9 l r) = show l ++ show r

instance Show Ex9' where
  show (Ex9' op l r) = show op ++ show l ++ show r
  show NIL9 = ""

data Ex10
  = Ex10
  { lhs10 :: Ex11,
    rhs10 :: Ex10'
  }
  deriving (Eq)

data Ex10' = Ex10' {op10 :: OP10, lhs'10 :: Ex11, rhs'10 :: Ex10'} | NIL10
  deriving (Eq)

instance Show Ex10 where
  show (Ex10 l r) = show l ++ show r

instance Show Ex10' where
  show (Ex10' op l r) = show op ++ show l ++ show r 
  show NIL10 = ""

data Ex11
  = Ex11
      { op11 :: OP11,
        rhs11 :: Ex11
      }
  | EX12 Ex12
  deriving (Eq)

instance Show Ex11 where
  show (Ex11 op r) =   show op ++ show r 
  show (EX12 ex) = show ex

data Ex12
  = Ex12
  { lhs12 :: Integer,
    rhs12 :: Ex12'
  }
  deriving (Eq)

data Ex12' = Ex12' {op12 :: OP12, lhs'12 :: Integer, rhs'12 :: Ex12'} | NIL12
  deriving (Eq)

instance Show Ex12 where
  show (Ex12 l r) = show l ++ show r

instance Show Ex12' where
  show (Ex12' op l r) = show op ++ show l ++ show r 
  show NIL12 = ""

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
