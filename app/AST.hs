module AST where

data OP1 = OR 
  deriving(Show, Eq)

data OP2 = AND
  deriving(Show, Eq)

data OP3 = LT | GT | LE | GE | NE | EQ
  deriving(Show, Eq)

data OP4 = B_OR
  deriving(Show, Eq)

data OP5 = B_XOR
  deriving(Show, Eq)

data OP6 = B_AND
  deriving(Show, Eq)

data OP7 = LSHIFT | RSHIFT
  deriving(Show, Eq)

data OP8 = CONCAT
  deriving(Show, Eq)

data OP9 = PLUS | MINUS
  deriving(Show, Eq)

data OP10 = MULT | DIV | INT_DIV | MOD
  deriving(Show, Eq)

data OP11 = NOT | INDEX | U_MINUS | B_NOT
  deriving(Show, Eq)

data OP12 = EXP
  deriving(Show, Eq)

data Ex1
  = Ex1
      { lhs1 :: Ex2,
        rhs1 :: Ex1'
      }
  deriving (Show, Eq)

data Ex1' = Ex1' {op1 :: OP1, lhs'1 :: Ex2, rhs'1 :: Ex1'} | NIL1
  deriving (Show, Eq)

data Ex2
  = Ex2
      { lhs2 :: Ex3,
        rhs2 :: Ex2'
      }
  deriving (Show, Eq)

data Ex2' = Ex2' {op2 :: OP2, lhs'2 :: Ex3, rhs'2 :: Ex2'} | NIL2
  deriving (Show, Eq)

data Ex3
  = Ex3
      { lhs3 :: Ex4,
        rhs3 :: Ex3'
      }
  deriving (Show, Eq)

data Ex3' = Ex3' {op3 :: OP3, lhs'3 :: Ex4, rhs'3 :: Ex3'} | NIL3
  deriving (Show, Eq)

data Ex4
  = Ex4
      { lhs4 :: Ex5,
        rhs4 :: Ex4'
      }
  deriving (Show, Eq)

data Ex4' = Ex4' {op4 :: OP4, lhs'4 :: Ex5, rhs'4 :: Ex4'} | NIL4
  deriving (Show, Eq)

data Ex5
  = Ex5
      { lhs5 :: Ex6,
        rhs5 :: Ex5'
      }
  deriving (Show, Eq)

data Ex5' = Ex5' {op5 :: OP5, lhs'5 :: Ex6, rhs'5 :: Ex5'} | NIL5
  deriving (Show, Eq)

data Ex6
  = Ex6
      { lhs6 :: Ex7,
        rhs6 :: Ex6'
      }
  deriving (Show, Eq)

data Ex6' = Ex6' {op6 :: OP6, lhs'6 :: Ex7, rhs'6 :: Ex6'} | NIL6
  deriving (Show, Eq)

data Ex7
  = Ex7
      { lhs7 :: Ex8,
        rhs7 :: Ex7'
      }
  deriving (Show, Eq)

data Ex7' = Ex7' {op7 :: OP7, lhs'7 :: Ex8, rhs'7 :: Ex7'} | NIL7
  deriving (Show, Eq)

data Ex8
  = Ex8
      { lhs8 :: Ex9,
        rhs8 :: Ex8'
      }
  deriving (Show, Eq)

data Ex8' = Ex8' {op8 :: OP8, lhs'8 :: Ex9, rhs'8 :: Ex8'} | NIL8
  deriving (Show, Eq)

data Ex9
  = Ex9
      { lhs9 :: Ex10,
        rhs9 :: Ex9'
      }
  deriving (Show, Eq)

data Ex9' = Ex9' {op9 :: OP9, lhs'9 :: Ex10, rhs'9 :: Ex9'} | NIL9
  deriving (Show, Eq)

data Ex10
  = Ex10
      { lhs10 :: Ex11,
        rhs10 :: Ex10'
      }
  deriving (Show, Eq)

data Ex10' = Ex10' {op10 :: OP10, lhs'10 :: Ex11, rhs'10 :: Ex10'} | NIL10
  deriving (Show, Eq)

data Ex11
  = Ex11
      { op11 :: OP11,
        rhs11 :: Ex11
      } | EX12 Ex12
  deriving (Show, Eq)

data Ex12
  = Ex12
      { lhs12 :: Integer,
        rhs12 :: Ex12'
      }
  deriving (Show, Eq)

data Ex12' = Ex12' {op12 :: OP12, lhs'12 :: Integer, rhs'12 :: Ex12'} | NIL12
  deriving (Show, Eq)
