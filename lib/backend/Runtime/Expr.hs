{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Runtime.Expr where

import qualified AST
import Data.Bits
import Data.Bits.Floating (FloatingBits (coerceToFloat), coerceToWord)
import Data.Text (pack)
import Data.Text.ICU (Collator, collate)
import Parser.Parser (num)
import Runtime.Types
import Text.Parsec (parse)

valFromLiteral :: AST.Literal -> Val
valFromLiteral (AST.StringLit x) = StringVal x
valFromLiteral (AST.NumLit (AST.NumDouble x)) = NumVal x
valFromLiteral (AST.NumLit (AST.NumInt x)) = NumVal $ fromIntegral x

coerce :: String -> Val
coerce str = case parse num "" str of
  Left _ -> error $ "Error: attempt to coerce string " ++ show str ++ " to a number"
  Right (AST.LiteralExpr x) -> valFromLiteral x
  _ -> undefined

arith :: (Double -> Double -> Double) -> Val -> Val -> Val
arith fn (NumVal x) (NumVal y) = NumVal $ fn x y
arith fn (StringVal x) (NumVal y) = NumVal $ fn a y
  where
    (NumVal a) = coerce x
arith fn (NumVal x) (StringVal y) = NumVal $ fn x b
  where
    (NumVal b) = coerce y
arith fn (StringVal x) (StringVal y) = NumVal $ fn a b
  where
    (NumVal a) = coerce x
    (NumVal b) = coerce y
arith _ _ _ = undefined

intDiv :: Val -> Val -> Val
intDiv (NumVal x) (NumVal y) = NumVal $ fromIntegral $ floor (x / y)
intDiv (StringVal x) (NumVal y) = NumVal $ fromIntegral $ floor (a / y)
  where
    (NumVal a) = coerce x
intDiv (NumVal x) (StringVal y) = NumVal $ fromIntegral $ floor (x / b)
  where
    (NumVal b) = coerce y
intDiv (StringVal x) (StringVal y) = NumVal $ fromIntegral $ floor (a / b)
  where
    (NumVal a) = coerce x
    (NumVal b) = coerce y
intDiv _ _ = undefined

valConcat :: Val -> Val -> Val
valConcat x y = StringVal $ show x ++ show y

valAnd :: Val -> Val -> Val
valAnd (BoolVal True) x = x
valAnd (BoolVal False) _ = BoolVal False
valAnd NilVal _ = NilVal
valAnd _ x = x

valOr :: Val -> Val -> Val
valOr (BoolVal True) _ = BoolVal True
valOr (BoolVal False) x = x
valOr NilVal x = x
valOr x _ = x

valEq :: Val -> Val -> Val
valEq x y = BoolVal $ x == y

valNe :: Val -> Val -> Val
valNe x y = BoolVal $ x /= y

numComp :: (Double -> Double -> Bool) -> Val -> Val -> Val
numComp fn (NumVal x) (NumVal y) = BoolVal $ fn x y
numComp _ _ _ = undefined

strOrd :: Collator -> Ordering -> Val -> Val -> Bool
strOrd collator ord (StringVal x) (StringVal y) = collate collator (pack x) (pack y) == ord
strOrd _ _ _ _ = undefined

valGt :: Collator -> Val -> Val -> Val
valGt collator (StringVal x) (StringVal y) = BoolVal $ strOrd collator GT (StringVal x) (StringVal y)
valGt _ x y = numComp (>) x y

valLt :: Collator -> Val -> Val -> Val
valLt collator (StringVal x) (StringVal y) = BoolVal $ strOrd collator LT (StringVal x) (StringVal y)
valLt _ x y = numComp (<) x y

valGe :: Collator -> Val -> Val -> Val
valGe collator (StringVal x) (StringVal y) = BoolVal $ strOrd collator GT (StringVal x) (StringVal y) || strOrd collator EQ (StringVal x) (StringVal y)
valGe _ x y = numComp (>=) x y

valLe :: Collator -> Val -> Val -> Val
valLe collator (StringVal x) (StringVal y) = BoolVal $ strOrd collator GT (StringVal x) (StringVal y) || strOrd collator EQ (StringVal x) (StringVal y)
valLe _ x y = numComp (<=) x y

valMod :: Val -> Val -> Val
valMod (NumVal x) (NumVal y) = NumVal $ fromIntegral $ floor x `mod` floor y
valMod (StringVal x) (NumVal y) = NumVal $ fromIntegral $ floor a `mod` floor y
  where
    (NumVal a) = coerce x
valMod (NumVal x) (StringVal y) = NumVal $ fromIntegral $ floor x `mod` floor b
  where
    (NumVal b) = coerce y
valMod (StringVal x) (StringVal y) = NumVal $ fromIntegral $ floor a `mod` floor b
  where
    (NumVal a) = coerce x
    (NumVal b) = coerce y
valMod _ _ = undefined

valBAnd :: Val -> Val -> Val
valBAnd (NumVal x) (NumVal y) = NumVal $ coerceToFloat $ coerceToWord x .&. coerceToWord y
valBAnd _ _ = undefined

valBOr :: Val -> Val -> Val
valBOr (NumVal x) (NumVal y) = NumVal $ coerceToFloat $ coerceToWord x .|. coerceToWord y
valBOr _ _ = undefined

valBXor :: Val -> Val -> Val
valBXor (NumVal x) (NumVal y) = NumVal $ coerceToFloat $ coerceToWord x `xor` coerceToWord y
valBXor _ _ = undefined

valLshift :: Val -> Val -> Val
valLshift (NumVal x) (NumVal y) = NumVal $ coerceToFloat $ coerceToWord x `shiftL` floor y
valLshift _ _ = undefined

valRshift :: Val -> Val -> Val
valRshift (NumVal x) (NumVal y) = NumVal $ coerceToFloat $ coerceToWord x `shiftR` floor y
valRshift _ _ = undefined

binOpToFn :: Collator -> AST.BIN_OP -> (Val -> Val -> Val)
binOpToFn collator x = case x of
  AST.OR -> valOr
  AST.AND -> valAnd
  AST.LE -> valLe collator
  AST.GE -> valGe collator
  AST.LT -> valLt collator
  AST.GT -> valGt collator
  AST.NE -> valNe
  AST.EQ -> valEq
  AST.B_OR -> valBOr
  AST.B_XOR -> valBXor
  AST.B_AND -> valBAnd
  AST.LSHIFT -> valLshift
  AST.RSHIFT -> valRshift
  AST.CONCAT -> valConcat
  AST.PLUS -> arith (+)
  AST.MINUS -> arith (-)
  AST.MULT -> arith (*)
  AST.DIV -> arith (/)
  AST.INT_DIV -> intDiv
  AST.MOD -> valMod
  AST.EXP -> arith (**)

valUNot :: Val -> Val
valUNot (BoolVal False) = BoolVal True
valUNot NilVal = BoolVal True
valUNot _ = BoolVal False

valUMinus :: Val -> Val
valUMinus (NumVal x) = NumVal (-x)
valUMinus (StringVal x) = NumVal (-a)
  where
    (NumVal a) = coerce x
valUMinus _ = undefined

valULen :: Val -> Val
valULen _ = undefined -- NEEDS TABLES

valUBNot :: Val -> Val
valUBNot (NumVal x) = NumVal $ fromIntegral $ complement $ coerceToWord x
valUBNot _ = undefined

unOpToFn :: AST.U_OP -> (Val -> Val)
unOpToFn x = case x of
  AST.U_NOT -> valUNot
  AST.U_MINUS -> valUMinus
  AST.U_LEN -> valULen
  AST.U_B_NOT -> valUBNot

interpretPE :: Env -> AST.PrefixExpr -> (Env, Val)
interpretPE x (AST.PrefixSub y ys)
  | y3 == NilVal = (x2, y2)
  | otherwise = (x3, y3)
  where
    (x2, y2) = interpretE x y
    (x3, y3) = interpretPE' x3 ys
interpretPE _ (AST.PrefixName {}) = undefined

interpretPE' :: Env -> AST.PrefixExpr' -> (Env, Val)
interpretPE' _ (AST.TableIndex {}) = undefined
interpretPE' _ (AST.DotIndex {}) = undefined
interpretPE' _ (AST.CallArgs {}) = undefined
interpretPE' _ (AST.MethodArgs {}) = undefined
interpretPE' x AST.PrefixEmpty = (x,NilVal)

interpretE :: Env -> AST.Expr -> (Env, Val)
interpretE x (AST.BinExpr a op b) = (x3, fn a2 b2)
  where
    (x2, a2) = interpretE x a
    fn = binOpToFn (collator x2) op
    (x3, b2) = interpretE x2 b
interpretE x (AST.UnaryExpr op y) = (x2, fn b)
  where
    fn = unOpToFn op
    (x2, b) = interpretE x y
interpretE x (AST.LiteralExpr y) = (x, valFromLiteral y)
interpretE x (AST.PreExpr y) = interpretPE x y
interpretE x AST.NIL = (x, NilVal)
interpretE x AST.TRUE = (x, BoolVal True)
interpretE x AST.FALSE = (x, BoolVal False)
interpretE _ AST.TRIPLE_DOT = undefined
interpretE _ (AST.TableExpr _) = undefined
interpretE _ (AST.FunctionDef _) = undefined

