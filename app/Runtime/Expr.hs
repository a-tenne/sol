{-# OPTIONS_GHC -Wno-type-defaults #-}

module Runtime.Expr where

import AST
import Runtime.Types

valFromLiteral :: Literal -> Val
valFromLiteral (StringLit x) = StringVal x
valFromLiteral (NumLit (NumDouble x)) = NumVal x
valFromLiteral (NumLit (NumInt x)) = NumVal $ fromIntegral x

arith :: (Double -> Double -> Double) -> Val -> Val -> Val
arith fn (NumVal x) (NumVal y) = NumVal $ fn x y
arith _ (StringVal _) _ = undefined
arith _ _ (StringVal _) = undefined
arith _ (BoolVal _) _ = undefined
arith _ _ (BoolVal _) = undefined
arith _ NilVal _ = undefined
arith _ _ NilVal = undefined

intDiv :: Val -> Val -> Val
intDiv (NumVal x) (NumVal y) = NumVal $ fromIntegral $ floor (x / y)
intDiv (StringVal _) _ = undefined
intDiv _ (StringVal _) = undefined
intDiv (BoolVal _) _ = undefined
intDiv _ (BoolVal _) = undefined
intDiv NilVal _ = undefined
intDiv _ NilVal = undefined

valConcat :: Val -> Val -> Val
valConcat x y = StringVal $ show x ++ show y

opToFn :: BIN_OP -> (Val -> Val -> Val)
opToFn x = case x of
  PLUS -> arith (+)
  MINUS -> arith (-)
  MULT -> arith (*)
  DIV -> arith (/)
  INT_DIV -> intDiv
  CONCAT -> valConcat
  _ -> undefined

interpretE :: Env -> Expr -> (Env, Val)
interpretE x (LiteralExpr y) = (x, valFromLiteral y)
interpretE x (BinExpr a op b) = (x3, fn a2 b2)
  where
    (x2, a2) = interpretE x a
    fn :: Val -> Val -> Val = opToFn op
    (x3, b2) = interpretE x2 b
interpretE x FALSE = (x, BoolVal False)
interpretE x TRUE = (x, BoolVal True)
interpretE x NIL = (x, NilVal)

