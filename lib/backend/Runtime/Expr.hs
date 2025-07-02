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

type BinFn = GlobalEnv -> Env -> Val -> Val -> IO (GlobalEnv, Env, Val)

arith :: (Double -> Double -> Double) -> BinFn
arith fn g l (NumVal x) (NumVal y) =return (g, l, NumVal $ fn x y)
arith fn g l (StringVal x) (NumVal y) =return (g, l, NumVal $ fn a y)
  where
    (NumVal a) = coerce x
arith fn g l(NumVal x) (StringVal y) =return (g, l, NumVal $ fn x b)
  where
    (NumVal b) = coerce y
arith fn g l(StringVal x) (StringVal y) =return (g, l, NumVal $ fn a b)
  where
    (NumVal a) = coerce x
    (NumVal b) = coerce y
arith _ _ _ _ _ = undefined

intDiv :: BinFn
intDiv g l (NumVal x) (NumVal y) =return (g,l,NumVal $ fromIntegral $ floor (x / y))
intDiv g l (StringVal x) (NumVal y) =return (g,l,NumVal $ fromIntegral $ floor (a / y))
  where
    (NumVal a) = coerce x
intDiv g l (NumVal x) (StringVal y) =return (g,l,NumVal $ fromIntegral $ floor (x / b))
  where
    (NumVal b) = coerce y
intDiv g l (StringVal x) (StringVal y) =return (g,l,NumVal $ fromIntegral $ floor (a / b))
  where
    (NumVal a) = coerce x
    (NumVal b) = coerce y
intDiv _ _ _ _ = undefined

valConcat :: BinFn
valConcat g l (StringVal x) (StringVal y) =return (g,l,StringVal $ show (StringVal x) ++ show (StringVal y))
valConcat g l (StringVal x) (NumVal y) =return (g,l,StringVal $ show (StringVal x) ++ show (NumVal y))
valConcat g l (NumVal x) (StringVal y) =return (g,l,StringVal $ show (NumVal x) ++ show (StringVal y))
valConcat g l (NumVal x) (NumVal y) =return (g,l,StringVal $ show (NumVal x) ++ show (NumVal y))
valConcat _ _ _ _ = undefined

valAnd :: BinFn
valAnd g l (BoolVal True) x =return (g,l,x)
valAnd g l (BoolVal False) _ =return (g,l,BoolVal False)
valAnd g l NilVal _ =return (g,l, NilVal)
valAnd g l _ x =return (g,l,x)

valOr :: BinFn
valOr g l(BoolVal True) _ =return(g,l, BoolVal True)
valOr g l (BoolVal False) x =return(g,l, x)
valOr g l NilVal x =return(g,l, x)
valOr g l x _ =return(g,l, x)

valEq :: BinFn
valEq g l x y =return(g,l, BoolVal $ x == y)

valNe :: BinFn
valNe g l x y =return(g,l, BoolVal $ x /= y)

numComp :: (Double -> Double -> Bool) -> BinFn
numComp fn g l(NumVal x) (NumVal y) =return (g,l,BoolVal $ fn x y)
numComp _ _ _ _ _ = undefined

strOrd :: GlobalEnv -> Ordering -> Val -> Val -> Bool
strOrd g ord (StringVal x) (StringVal y) = collate (collator g) (pack x) (pack y) == ord
strOrd _ _ _ _ = undefined

valGt :: BinFn
valGt g l (StringVal x) (StringVal y) =return(g,l, BoolVal $ strOrd g GT (StringVal x) (StringVal y))
valGt g l x y = numComp (>) g l x y

valLt :: BinFn
valLt g l (StringVal x) (StringVal y) =return(g,l, BoolVal $ strOrd g LT (StringVal x) (StringVal y))
valLt g l x y = numComp (<) g l x y 

valGe :: BinFn
valGe g l (StringVal x) (StringVal y) =return(g,l, BoolVal $ strOrd g GT (StringVal x) (StringVal y) || strOrd g EQ (StringVal x) (StringVal y))
valGe g l x y = numComp (>=) g l x y

valLe :: BinFn
valLe g l (StringVal x) (StringVal y) =return(g,l, BoolVal $ strOrd g GT (StringVal x) (StringVal y) || strOrd g EQ (StringVal x) (StringVal y))
valLe g l x y = numComp (<=) g l x y

valMod :: BinFn
valMod g l (NumVal x) (NumVal y) =return(g,l, NumVal $ fromIntegral $ floor x `mod` floor y)
valMod g l (StringVal x) (NumVal y) =return(g,l, NumVal $ fromIntegral $ floor a `mod` floor y)
  where
    (NumVal a) = coerce x
valMod g l (NumVal x) (StringVal y) =return(g,l, NumVal $ fromIntegral $ floor x `mod` floor b)
  where
    (NumVal b) = coerce y
valMod g l (StringVal x) (StringVal y) =return(g,l, NumVal $ fromIntegral $ floor a `mod` floor b)
  where
    (NumVal a) = coerce x
    (NumVal b) = coerce y
valMod _ _ _ _ = undefined

valBAnd :: BinFn
valBAnd g l (NumVal x) (NumVal y) =return(g,l, NumVal $ coerceToFloat $ coerceToWord x .&. coerceToWord y)
valBAnd _ _ _ _ = undefined

valBOr :: BinFn
valBOr g l (NumVal x) (NumVal y) =return(g,l, NumVal $ coerceToFloat $ coerceToWord x .|. coerceToWord y)
valBOr _ _ _ _ = undefined

valBXor :: BinFn
valBXor g l (NumVal x) (NumVal y) =return(g,l, NumVal $ coerceToFloat $ coerceToWord x `xor` coerceToWord y)
valBXor _ _ _ _ = undefined

valLshift :: BinFn
valLshift g l (NumVal x) (NumVal y) =return(g,l, NumVal $ coerceToFloat $ coerceToWord x `shiftL` floor y)
valLshift _ _ _ _ = undefined

valRshift :: BinFn
valRshift g l (NumVal x) (NumVal y) =return(g,l, NumVal $ coerceToFloat $ coerceToWord x `shiftR` floor y)
valRshift _ _ _ _ = undefined

binOpToFn :: GlobalEnv -> Env -> AST.BIN_OP -> BinFn
binOpToFn g l op = case op of
  AST.OR -> valOr
  AST.AND -> valAnd
  AST.LE -> valLe
  AST.GE -> valGe
  AST.LT -> valLt
  AST.GT -> valGt
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

type UnFn = GlobalEnv -> Env -> Val -> IO(GlobalEnv, Env, Val)

valUNot :: UnFn
valUNot g l (BoolVal False) =return(g,l, BoolVal True)
valUNot g l NilVal =return(g,l, BoolVal True)
valUNot g l _ =return(g,l, BoolVal False)

valUMinus :: UnFn
valUMinus  g l(NumVal x) =return(g,l, NumVal (-x))
valUMinus  g l(StringVal x) =return(g,l, NumVal (-a))
  where
    (NumVal a) = coerce x
valUMinus _ _ _ = undefined

valULen :: UnFn
valULen = undefined -- NEEDS TABLES

valUBNot :: UnFn
valUBNot g l (NumVal x) =return(g,l, NumVal $ fromIntegral $ complement $ coerceToWord x)
valUBNot _ _ _ = undefined

unOpToFn :: AST.U_OP -> UnFn
unOpToFn op = case op of
  AST.U_NOT -> valUNot
  AST.U_MINUS -> valUMinus
  AST.U_LEN -> valULen
  AST.U_B_NOT -> valUBNot

interpretPE :: GlobalEnv -> Env -> AST.PrefixExpr -> IO(GlobalEnv, Env, Val)
interpretPE g l (AST.PrefixSub y ys) = do
    (g2,l2, y2) <- interpretE g l y
    (g3,l3, y3) <- interpretPE' g2 l2 ys
    if y3 == NilVal 
      then return (g2, l2, y2)
      else return (g3, l3, y3)
interpretPE _ _ (AST.PrefixName {}) = undefined

interpretPE' :: GlobalEnv -> Env -> AST.PrefixExpr' ->IO (GlobalEnv, Env, Val)
interpretPE' _ _ (AST.TableIndex {}) = undefined
interpretPE' _ _ (AST.DotIndex {}) = undefined
interpretPE' _ _ (AST.CallArgs {}) = undefined
interpretPE' _ _ (AST.MethodArgs {}) = undefined
interpretPE' g l AST.PrefixEmpty = return (g,l,NilVal)

interpretE :: GlobalEnv -> Env -> AST.Expr -> IO (GlobalEnv, Env, Val)
interpretE g l (AST.BinExpr a op b) = do 
  (g2,l2, a2) <- interpretE g l a
  (g3,l3, b2) <- interpretE g2 l2 b
  let fn = binOpToFn g3 l3 op
  fn g3 l3 a2 b2
interpretE g l (AST.UnaryExpr op y) = do
  (g2, l2, b) <- interpretE g l y
  let fn = unOpToFn op
  fn g2 l2 b
interpretE g l (AST.LiteralExpr y) =return (g,l, valFromLiteral y)
interpretE g l (AST.PreExpr y) = interpretPE g l y
interpretE g l AST.NIL =return (g,l, NilVal)
interpretE g l AST.TRUE =return (g,l, BoolVal True)
interpretE g l AST.FALSE =return (g,l, BoolVal False)
interpretE _ _ AST.TRIPLE_DOT = undefined
interpretE _ _ (AST.TableExpr _) = undefined
interpretE _ _ (AST.FunctionDef _) = undefined

interpretEL :: GlobalEnv -> Env -> AST.ExprList -> IO (GlobalEnv, Env, [Val])
interpretEL g l (AST.ExprList []) = return (g,l,[])
interpretEL g l (AST.ExprList (x:xs)) = do
  (g2, l2, v) <- interpretE g l x
  (g3, l3, vl) <- interpretEL g2 l2 (AST.ExprList xs)
  return (g3,l3, v:vl)
