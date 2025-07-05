{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Runtime.Interpreter where

import AST
import Control.Monad (void)
import Data.Bits
import Data.Bits.Floating (FloatingBits (coerceToFloat), coerceToWord)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Text.ICU (Collator, collate)
import Data.Unique (Unique, newUnique)
import Parser.Parser (num)
import Runtime.Runtime
import Runtime.Types
import Text.Parsec (parse)
import Data.IORef (readIORef, newIORef)

valFromLiteral :: Literal -> Val
valFromLiteral (StringLit x) = StringVal x
valFromLiteral (NumLit (NumDouble x)) = NumVal x
valFromLiteral (NumLit (NumInt x)) = NumVal $ fromIntegral x

coerce :: String -> Val
coerce str = case parse num "" str of
  Left _ -> error $ "attempt to coerce string " ++ show str ++ " to a number"
  Right (LiteralExpr x) -> valFromLiteral x

type BinFn = GlobalEnv -> Env -> Val -> Val -> IO (GlobalEnv, Env, Val)

arith :: (Double -> Double -> Double) -> BinFn
arith fn g l (NumVal x) (NumVal y) = return (g, l, NumVal $ fn x y)
arith fn g l (StringVal x) (NumVal y) = return (g, l, NumVal $ fn a y)
  where
    (NumVal a) = coerce x
arith fn g l (NumVal x) (StringVal y) = return (g, l, NumVal $ fn x b)
  where
    (NumVal b) = coerce y
arith fn g l (StringVal x) (StringVal y) = return (g, l, NumVal $ fn a b)
  where
    (NumVal a) = coerce x
    (NumVal b) = coerce y
arith _ _ _ x y = error $ "tried to perform illegal arithmetic operation between " ++ show x ++ " and " ++ show y

intDiv :: BinFn
intDiv g l (NumVal x) (NumVal y) = return (g, l, NumVal $ fromIntegral $ floor (x / y))
intDiv g l (StringVal x) (NumVal y) = return (g, l, NumVal $ fromIntegral $ floor (a / y))
  where
    (NumVal a) = coerce x
intDiv g l (NumVal x) (StringVal y) = return (g, l, NumVal $ fromIntegral $ floor (x / b))
  where
    (NumVal b) = coerce y
intDiv g l (StringVal x) (StringVal y) = return (g, l, NumVal $ fromIntegral $ floor (a / b))
  where
    (NumVal a) = coerce x
    (NumVal b) = coerce y
intDiv _ _ x y = error $ "tried to perform illegal arithmetic operation between " ++ show x ++ " and " ++ show y

valConcat :: BinFn
valConcat g l (StringVal x) (StringVal y) = return (g, l, StringVal $ show (StringVal x) ++ show (StringVal y))
valConcat g l (StringVal x) (NumVal y) = return (g, l, StringVal $ show (StringVal x) ++ show (NumVal y))
valConcat g l (NumVal x) (StringVal y) = return (g, l, StringVal $ show (NumVal x) ++ show (StringVal y))
valConcat g l (NumVal x) (NumVal y) = return (g, l, StringVal $ show (NumVal x) ++ show (NumVal y))
valConcat _ _ x y = error $ "tried to perform illegal concatenation between " ++ show x ++ " and " ++ show y

valAnd :: BinFn
valAnd g l (BoolVal True) x = return (g, l, x)
valAnd g l (BoolVal False) _ = return (g, l, BoolVal False)
valAnd g l NilVal _ = return (g, l, NilVal)
valAnd g l _ x = return (g, l, x)

valOr :: BinFn
valOr g l (BoolVal True) _ = return (g, l, BoolVal True)
valOr g l (BoolVal False) x = return (g, l, x)
valOr g l NilVal x = return (g, l, x)
valOr g l x _ = return (g, l, x)

valEq :: BinFn
valEq g l x y = return (g, l, BoolVal $ x == y)

valNe :: BinFn
valNe g l x y = return (g, l, BoolVal $ x /= y)

numComp :: (Double -> Double -> Bool) -> BinFn
numComp fn g l (NumVal x) (NumVal y) = return (g, l, BoolVal $ fn x y)
numComp _ _ _ x y = error $ "tried to perform illegal comparison between " ++ show x ++ " and " ++ show y

strOrd :: GlobalEnv -> Ordering -> Val -> Val -> Bool
strOrd g ord (StringVal x) (StringVal y) = collate (collator g) (pack x) (pack y) == ord
strOrd _ _ x y = error $ "tried to perform illegal comparison between " ++ show x ++ " and " ++ show y

valGt :: BinFn
valGt g l (StringVal x) (StringVal y) = return (g, l, BoolVal $ strOrd g Prelude.GT (StringVal x) (StringVal y))
valGt g l x y = numComp (>) g l x y

valLt :: BinFn
valLt g l (StringVal x) (StringVal y) = return (g, l, BoolVal $ strOrd g Prelude.LT (StringVal x) (StringVal y))
valLt g l x y = numComp (<) g l x y

valGe :: BinFn
valGe g l (StringVal x) (StringVal y) = return (g, l, BoolVal $ strOrd g Prelude.GT (StringVal x) (StringVal y) || strOrd g Prelude.EQ (StringVal x) (StringVal y))
valGe g l x y = numComp (>=) g l x y

valLe :: BinFn
valLe g l (StringVal x) (StringVal y) = return (g, l, BoolVal $ strOrd g Prelude.GT (StringVal x) (StringVal y) || strOrd g Prelude.EQ (StringVal x) (StringVal y))
valLe g l x y = numComp (<=) g l x y

valMod :: BinFn
valMod g l (NumVal x) (NumVal y) = return (g, l, NumVal $ fromIntegral $ floor x `mod` floor y)
valMod g l (StringVal x) (NumVal y) = return (g, l, NumVal $ fromIntegral $ floor a `mod` floor y)
  where
    (NumVal a) = coerce x
valMod g l (NumVal x) (StringVal y) = return (g, l, NumVal $ fromIntegral $ floor x `mod` floor b)
  where
    (NumVal b) = coerce y
valMod g l (StringVal x) (StringVal y) = return (g, l, NumVal $ fromIntegral $ floor a `mod` floor b)
  where
    (NumVal a) = coerce x
    (NumVal b) = coerce y
valMod _ _ x y = error $ "tried to perform illegal arithmetic operation between " ++ show x ++ " and " ++ show y

valBAnd :: BinFn
valBAnd g l (NumVal x) (NumVal y) = return (g, l, NumVal $ coerceToFloat $ coerceToWord x .&. coerceToWord y)
valBAnd _ _ x y = error $ "tried to perform illegal binary operation between " ++ show x ++ " and " ++ show y

valBOr :: BinFn
valBOr g l (NumVal x) (NumVal y) = return (g, l, NumVal $ coerceToFloat $ coerceToWord x .|. coerceToWord y)
valBOr _ _ x y = error $ "tried to perform illegal binary operation between " ++ show x ++ " and " ++ show y

valBXor :: BinFn
valBXor g l (NumVal x) (NumVal y) = return (g, l, NumVal $ coerceToFloat $ coerceToWord x `xor` coerceToWord y)
valBXor _ _ x y = error $ "tried to perform illegal binary operation between " ++ show x ++ " and " ++ show y

valLshift :: BinFn
valLshift g l (NumVal x) (NumVal y) = return (g, l, NumVal $ coerceToFloat $ coerceToWord x `shiftL` floor y)
valLshift _ _ x y = error $ "tried to perform illegal binary operation between " ++ show x ++ " and " ++ show y

valRshift :: BinFn
valRshift g l (NumVal x) (NumVal y) = return (g, l, NumVal $ coerceToFloat $ coerceToWord x `shiftR` floor y)
valRshift _ _ x y = error $ "tried to perform illegal binary operation between " ++ show x ++ " and " ++ show y

valIsTrue :: Val -> Bool
valIsTrue (BoolVal False) = False
valIsTrue NilVal = False
valIsTrue _ = True

binOpToFn :: GlobalEnv -> Env -> BIN_OP -> BinFn
binOpToFn g l op = case op of
  OR -> valOr
  AND -> valAnd
  LE -> valLe
  GE -> valGe
  AST.LT -> valLt
  AST.GT -> valGt
  NE -> valNe
  AST.EQ -> valEq
  B_OR -> valBOr
  B_XOR -> valBXor
  B_AND -> valBAnd
  LSHIFT -> valLshift
  RSHIFT -> valRshift
  CONCAT -> valConcat
  PLUS -> arith (+)
  MINUS -> arith (-)
  MULT -> arith (*)
  DIV -> arith (/)
  INT_DIV -> intDiv
  MOD -> valMod
  EXP -> arith (**)

type UnFn = GlobalEnv -> Env -> Val -> IO (GlobalEnv, Env, Val)

valUNot :: UnFn
valUNot g l (BoolVal False) = return (g, l, BoolVal True)
valUNot g l NilVal = return (g, l, BoolVal True)
valUNot g l _ = return (g, l, BoolVal False)

valUMinus :: UnFn
valUMinus g l (NumVal x) = return (g, l, NumVal (-x))
valUMinus g l (StringVal x) = return (g, l, NumVal (-a))
  where
    (NumVal a) = coerce x
valUMinus _ _ x = error $ "tried to perform illegal operation -(" ++ show x ++ ")"

valULen :: UnFn
valULen g l (TableVal _ tRef) = do
  (Table m) <- readIORef tRef
  return (g, l, NumVal $ fromIntegral $ length m)
valULen _ _ x = error $ "tried to perform a length operation on a non table value: " ++ show x

valUBNot :: UnFn
valUBNot g l (NumVal x) = return (g, l, NumVal $ fromIntegral $ complement $ coerceToWord x)
valUBNot _ _ x = error $ "tried to perform illegal operation ~(" ++ show x ++ ")"

unOpToFn :: U_OP -> UnFn
unOpToFn op = case op of
  U_NOT -> valUNot
  U_MINUS -> valUMinus
  U_LEN -> valULen
  U_B_NOT -> valUBNot

interpretPE :: GlobalEnv -> Env -> PrefixExpr -> IO (GlobalEnv, Env, Maybe (Unique, [Val]), [Val])
-- Evaluates the subexpression. If it's a table, passes down the table identifier to the next function
interpretPE g l (PrefixSub y ys) = do
  (g2, l2, vl) <- interpretE g l y
  case vl of
    ((TableVal u _) : _) -> interpretPE' g2 l2 (Just (u, [])) vl ys
    _ -> interpretPE' g2 l2 Nothing vl ys

-- Gets the variable assigned to the name expression or nil. If it's a table, passes down the table identifier to the next function
interpretPE g l (PrefixName y ys) = do
  let v = getVar g l y
  case v of
    (TableVal u _) -> interpretPE' g l (Just (u, [])) [v] ys
    _ -> interpretPE' g l Nothing [v] ys

interpretPE' :: GlobalEnv -> Env -> Maybe (Unique, [Val]) -> [Val] -> PrefixExpr' -> IO (GlobalEnv, Env, Maybe (Unique, [Val]), [Val])
-- Indexes a table. Adds the value used to index the table into the value list, if this is a table access chain.
interpretPE' g l uid (v : _) (TableIndex y ys) = do
  case v of
    (TableVal _ tRef) -> do
      (g2, l2, v2 : _) <- interpretE g l y
      t <- readIORef tRef
      let val = tableLookup t v2
      let newuid = case uid of
            Just (u, ul) -> Just (u, ul ++ [v2])
            Nothing -> error "internal error"
      interpretPE' g2 l2 newuid [val] ys
    _ -> error $ "tried to index a non table value: " ++ show v

-- Indexes a table. Adds the string value used to index the table into the value list, if this is a table access chain.
interpretPE' g l uid (v : _) (DotIndex y ys) = do
  case v of
    (TableVal _ tRef) -> do
      t <- readIORef tRef
      let val = tableLookup t (StringVal y)
      let newuid = case uid of
            Just (u, ul) -> Just (u, ul ++ [StringVal y])
            Nothing -> error "internal error"
      interpretPE' g l newuid [val] ys
    _ -> error $ "tried to index a non table value: " ++ show v

-- Uid is irrelevant now, since we're calling a function.
interpretPE' g l _ (v : _) (CallArgs y ys) = do
  (g2, l2, v2 : vl2) <- interpretArgs g l v Nothing y
  case v2 of
    (TableVal u _) -> interpretPE' g l (Just (u, [])) (v2 : vl2) ys
    _ -> interpretPE' g2 l2 Nothing (v2 : vl2) ys
interpretPE' g l _ (v:_) (MethodArgs n y ys) = do
  case v of
      (TableVal uniq tRef) -> do
        t <- readIORef tRef
        let fn = tableLookup t (StringVal n)
        (g2,l2,v2:vl2) <- interpretArgs g l fn (Just v) y
        case v2 of
          (TableVal u _) -> interpretPE' g l (Just (u, [])) (v2 : vl2) ys
          _ -> interpretPE' g2 l2 Nothing (v2 : vl2) ys
      _ -> error $ "tried to index a non table value: " ++ show v

interpretPE' g l uid vl PrefixEmpty = return (g, l, uid, vl)

-- Check if input value is a function. If it is, create a new environment
interpretArgs :: GlobalEnv -> Env -> Val -> Maybe Val -> Args -> IO (GlobalEnv, Env, [Val])
interpretArgs g l v methodCaller (ArgList el) = do
  (g2, l2, vl) <- interpretEL g l el
  let vl2 = case methodCaller of
        Nothing -> vl
        Just self -> self : vl
  -- Need to substitute any void values for nil before calling, except for the last one
  (g3, l3, vl3) <- case v of
    (FuncVal _ numArgs func) -> do
      let lenDiff = numArgs - length vl2
      let formattedVals = cleanVals vl2 ++ replicate lenDiff NilVal
      func g2 l2 formattedVals
    _ -> error $ show v ++ " is not a function."
  return (g3, l2, vl3)
interpretArgs g l v methodCaller (ArgString e) = do
  (g2, l2, vl) <- interpretE g l e
  let vl2 = case methodCaller of
        Nothing -> vl
        Just self -> self : vl
  (g3, l3, vl3) <- case v of
    (FuncVal _ numArgs func) -> func g2 l2 vl2
    _ -> error $ show v ++ " is not a function."
  return (g3, l2, vl3)
interpretArgs g l v methodCaller (ArgTable (TableConstructor fl)) = do
  (g2, l2, t) <- interpretFields g l fl
  tName <- newUnique
  tRef <- newIORef t
  let tableVal = TableVal tName tRef
  let vl = case methodCaller of
        Nothing -> [tableVal]
        Just self -> self : [tableVal]
  (g3, l3, vl2) <- case v of
    (FuncVal _ numArgs func) -> func g2 l2 vl
    _ -> error $ show v ++ " is not a function."
  return (g3, l2, vl2)

interpretFieldsHelper :: GlobalEnv -> Env -> Table -> [Field] -> IO (GlobalEnv, Env, Table)
interpretFieldsHelper g l t [] = return (g, l, t)
interpretFieldsHelper g l (Table m) (x : xs) = do
  case x of
    (ExField e1 e2) -> do
      (g2, l2, v1 : _) <- interpretE g l e1
      (g3, l3, v2 : _) <- interpretE g2 l2 e2
      interpretFieldsHelper g3 l3 (Table (M.insert v1 v2 m)) xs
    (NamedField n e) -> do
      (g2, l2, v : _) <- interpretE g l e
      interpretFieldsHelper g2 l2 (Table (M.insert (StringVal n) v m)) xs
    (SingleExField e) -> do
      let nextFree = getNextFreeIndex (Table m) 1
      (g2, l2, v : _) <- interpretE g l e
      interpretFieldsHelper g2 l2 (Table (M.insert (NumVal $ fromIntegral nextFree) v m)) xs
  where
    getNextFreeIndex :: Table -> Int -> Int
    getNextFreeIndex (Table m) i = case M.lookup (NumVal $ fromIntegral i) m of
      Just _ -> getNextFreeIndex (Table m) (i + 1)
      Nothing -> i

interpretFields :: GlobalEnv -> Env -> [Field] -> IO (GlobalEnv, Env, Table)
interpretFields g l fl = do
  let t = Table M.empty
  interpretFieldsHelper g l t fl

interpretE :: GlobalEnv -> Env -> Expr -> IO (GlobalEnv, Env, [Val])
interpretE g l (BinExpr a op b) = do
  (g2, l2, a2 : _) <- interpretE g l a
  (g3, l3, b2 : _) <- interpretE g2 l2 b
  let fn = binOpToFn g3 l3 op
  (g4, l4, v) <- fn g3 l3 a2 b2
  return (g4, l4, [v])
interpretE g l (UnaryExpr op y) = do
  (g2, l2, b : _) <- interpretE g l y
  let fn = unOpToFn op
  (g3, l3, v) <- fn g2 l2 b
  return (g3, l3, [v])
interpretE g l (LiteralExpr y) = return (g, l, [valFromLiteral y])
interpretE g l (PreExpr y) = do
  (g2, l2, _, vl) <- interpretPE g l y
  return (g2, l2, vl)
interpretE g l NIL = return (g, l, [NilVal])
interpretE g l TRUE = return (g, l, [BoolVal True])
interpretE g l FALSE = return (g, l, [BoolVal False])
interpretE g l TRIPLE_DOT = do
  case getVarArgs l of
    Just varArgs -> return (g, l, varArgs)
    Nothing -> error "tried to use varargs inside a non vararg function"
interpretE g l (TableExpr (TableConstructor fl)) = do
  (g2, l2, t) <- interpretFields g l fl
  tRef <- newIORef t
  tName <- newUnique
  return (g2, l2, [TableVal tName tRef])
interpretE g l (FunctionDef fb) = do
  let (FuncBody (ParamList (NameList nl) _) _) = fb
  let numArgs = length nl
  uniq <- newUnique
  return (g, l, [FuncVal uniq numArgs (luaFunc fb)])

interpretEL :: GlobalEnv -> Env -> ExprList -> IO (GlobalEnv, Env, [Val])
interpretEL g l (ExprList []) = return (g, l, [])
interpretEL g l (ExprList (x : xs)) = do
  (g2, l2, vl1) <- interpretE g l x
  (g3, l3, vl2) <- interpretEL g2 l2 (ExprList xs)
  return (g3, l3, vl1 ++ vl2)

interpret :: AST -> IO ()
interpret (AST ch) = interpretCh ch

interpretCh :: Chunk -> IO ()
interpretCh (Chunk b) = do
  g <- initialEnv
  void $ interpretB g EnvEmpty b

interpretB :: GlobalEnv -> Env -> Block -> IO (GlobalEnv, Env, Maybe [Val])
interpretB g l (Block sl rs) = do
  (g2, l2, ret) <- interpretSL g l sl
  case ret of
    -- Case: statement list returned, stop here
    Just _ -> return (g2, l2, ret)
    -- Case: statement list didn't return, check block's return
    Nothing -> case rs of
      -- Case: there is a return. Interpret EL and return values
      Just (RetStat el) -> do
        (g3, l3, vl) <- interpretEL g2 l2 el
        return (g3, l3, Just vl)
      -- Case: We have no return whatsoever
      Nothing -> return (g2, l2, Nothing)

-- Take in the global and local environments, process that list of statements.
-- If there is a "return" statement at any point, and only then, the last element
-- of the tuple becomes "Just [Val]" and at that point, the stack must be unwinded
interpretSL :: GlobalEnv -> Env -> StatList -> IO (GlobalEnv, Env, Maybe [Val])
interpretSL g l (AST.StatList []) = return (g, l, Nothing)
-- Returns VoidVal so the stack unwinds.
interpretSL g l (AST.StatList (Break : _)) = return (g, l, Just [VoidVal])
-- Discards the rest of the SL, SL in label becomes new SL
interpretSL g l (AST.StatList ((Goto n) : _)) = do
  let v = lookupVar g l n
  case v of
    Just x -> case x of
      (Left g2, v2) ->
        if l /= EnvEmpty
          then labelError
          else case v2 of
            (LabelVal sl) -> interpretSL g2 l sl
            _ -> labelError
      (Right l2, v2) ->
        if l /= l2
          then labelError
          else case v2 of
            (LabelVal sl) -> interpretSL g l2 sl
            _ -> labelError
    Nothing -> labelError
  where
    labelError = error $ "Goto label " ++ n ++ " not found"
-- Creates new label
interpretSL g l (AST.StatList ((Label n) : xs)) = do
  let (g2, l2) = insertVarLocal g l n (LabelVal (AST.StatList xs))
  interpretSL g2 l2 (AST.StatList xs)

-- Checks if the interpreted statement contained a return. If so, stop. If not, keep going
interpretSL g l (AST.StatList (x : xs)) = do
  (g2, l2, ret) <- interpretS g l x
  case ret of
    Nothing -> interpretSL g2 l2 (AST.StatList xs)
    Just _ -> return (g2, l2, ret)

interpretS :: GlobalEnv -> Env -> Stat -> IO (GlobalEnv, Env, Maybe [Val])
-- Semicolons are NO-OP
interpretS g l Semic = return (g, l, Nothing)
-- Global assignment (or general reassignment) of vars
interpretS g l (Asgn (VarList varl) el) = do
  (g2, l2, vl) <- interpretEL g l el
  -- In case there's void values here, turn them into nil
  let cleanVl = cleanVals vl
  (g3, l3, finalVarL) <- processVarL g2 l2 varl
  (g4, l4) <- updateValues g3 l3 finalVarL cleanVl
  return (g4, l4, Nothing)
  where
    processVariable :: GlobalEnv -> Env -> Var -> IO (GlobalEnv, Env, Either Name (Unique, [Val]))
    processVariable g l (Var (PrefixName n PrefixEmpty)) = return (g, l, Left n)
    processVariable g l (Var x) = do
      (g2, l2, uid, _) <- interpretPE g l x
      case uid of
        Just x -> return (g2, l2, Right x)
        Nothing -> error "internal error"
    processVarL :: GlobalEnv -> Env -> [Var] -> IO (GlobalEnv, Env, [Either Name (Unique, [Val])])
    processVarL g l [v] = do
      (g2, l2, v2) <- processVariable g l v
      return (g2, l2, [v2])
    processVarL g l (v : vs) = do
      (g2, l2, v2) <- processVariable g l v
      (g3, l3, vTail) <- processVarL g2 l2 vs
      return (g3, l3, v2 : vTail)
    processVarL g l [] = error "internal error"
    updateValue :: GlobalEnv -> Env -> Either Name (Unique, [Val]) -> Val -> IO(GlobalEnv, Env)
    updateValue g l (Left n) v = case lookupVar g l n of
        Just (Right _, _) -> return $ insertVarLocal g l n v
        _ -> return (insertVarGlobal g n v,l)
    updateValue g l (Right uid) v = updateTableVal g l uid v >> return (g,l)
    updateValues :: GlobalEnv -> Env -> [Either Name (Unique, [Val])] -> [Val] -> IO(GlobalEnv, Env)
    updateValues g l [] [] = return (g, l)
    updateValues g l [] _ = return (g, l)
    updateValues g l (x : xs) [] = do
      (g2, l2) <- updateValue g l x NilVal 
      updateValues g2 l2 xs []
    updateValues g l (x : xs) (y : ys) = do 
      (g2, l2) <- updateValue g l x y 
      updateValues g2 l2 xs ys
interpretS g l (Do b) = interpretB g l b
interpretS g l (LocalAsgn (AttrNameList attrnL) mEl) = do
  let (nl, _) = unzip attrnL -- Note: attributes don't do anything yet
  (g2, l2, vl) <- case mEl of
    Nothing -> return (g, l, [NilVal])
    Just x -> interpretEL g l x
  let lenDiff = length nl - length vl
  if lenDiff > 0
    then do
      let vl2 = vl ++ replicate lenDiff NilVal
      let (g3, l3) = insertVarsCurrent g2 l2 nl vl2
      return (g3, l3, Nothing)
    else do
      let (g3, l3) = insertVarsCurrent g2 l2 nl vl
      return (g3, l3, Nothing)
interpretS g l (FuncCallStat (FuncCall ex)) = do
  (g2, l2, _, _) <- interpretPE g l ex
  return (g2, l2, Nothing)
interpretS g l (ForStat n e1 e2 me b) = do
  (g2, l2, start : _) <- interpretE g l e1
  (g3, l3, end : _) <- interpretE g2 l2 e2
  (g4, l4, incr : _) <- case me of
    Just x -> interpretE g3 l3 x
    Nothing -> return (g3, l3, [NumVal 1])
  assertNumber start >> assertNumber end >> assertNumber incr
  -- insert index into env
  let (g5, l5) = insertVarCurrent g4 (newLocalEnv (Just l4)) n start
  -- run loop
  -- Note: the loop creates an own environment on each iteration
  (g6, l6, ret) <- forIter g5 l5 n end incr b
  -- gets updated parent env
  let l7 = case getParent l6 of
        Just x -> x
        Nothing -> error "Internal error"
  -- return updated local env without index and new global env
  return (g6, l7, ret)
  where
    assertNumber :: Val -> IO ()
    assertNumber (NumVal _) = return ()
    assertNumber x = error $ "for loop index value " ++ show x ++ " is not a number"
interpretS g l (WhileDo e b) = do
  (g2, l2, v : _) <- interpretE g l e
  if valIsTrue v
    then do
      let loopEnv = newLocalEnv (Just l2)
      (g3, loopEnv2, ret) <- interpretB g2 loopEnv b
      let l3 = case getParent loopEnv2 of
            Just x -> x
            Nothing -> error "Internal error"
      case ret of
        Just _ -> return (g3, l3, ret)
        Nothing -> interpretS g3 l3 (WhileDo e b)
    else
      return (g2, l2, Nothing)
interpretS g l (RepeatUntil b e) = do
  let loopEnv = newLocalEnv (Just l)
  (g2, loopEnv2, ret) <- interpretB g loopEnv b
  let l2 = case getParent loopEnv2 of
        Just x -> x
        Nothing -> error "Internal error"
  case ret of
    Just _ -> return (g2, l2, ret)
    Nothing -> do
      (g3, l3, v : _) <- interpretE g2 l2 e
      if valIsTrue v
        then return (g, l, Nothing)
        else interpretS g3 l3 (RepeatUntil b e)
interpretS g l (IfStat cond1 b (ElseIfList elifl) mElse) = do
  (g2,l2,v1:_) <- interpretE g l cond1
  if valIsTrue v1
    then interpretIfBlock g2 l2 b
    else do
      (g3,l3, mBlock) <- validateElifL g2 l2 elifl
      case mBlock of
        Just elifb -> interpretIfBlock g3 l3 elifb
        Nothing -> case mElse of
            Just (Else elseb) -> interpretIfBlock g3 l3 elseb
            Nothing -> return (g3,l3,Nothing)
  where
    interpretIfBlock :: GlobalEnv -> Env -> Block -> IO(GlobalEnv, Env, Maybe [Val])
    interpretIfBlock g l b = do
      let ifEnv = newLocalEnv (Just l)
      (g2,ifEnv2,retStat) <- interpretB g ifEnv b
      let l2 = fromMaybe EnvEmpty $ getParent ifEnv2
      return (g2,l2,retStat)
    validateElif :: GlobalEnv -> Env -> ElseIf -> IO(GlobalEnv, Env, Bool)
    validateElif g l (ElseIf cond b)= do
      (g2,l2,v:_) <- interpretE g l cond
      if valIsTrue v
        then
          return (g2,l2,True)
        else
          return (g2,l2,False)
    validateElifL :: GlobalEnv -> Env -> [ElseIf] -> IO(GlobalEnv, Env, Maybe Block)
    validateElifL g l [] = return (g,l,Nothing)
    validateElifL g l ((ElseIf cond b):xs) = do
      (g2,l2,condSatisfied) <- validateElif g l (ElseIf cond b)
      if condSatisfied
        then
          return (g2,l2, Just b)
        else
          validateElifL g2 l2 xs

interpretS _ _ (ForIn {}) = error "sol does not currently support for ... in statements"
interpretS g l (FuncDefStat (FuncName n nl maybeN) (FuncBody (ParamList (NameList nlFn)varArg)b)) = do
  let vl = map StringVal $ case maybeN of
        Just x -> nl ++ [x]
        Nothing -> nl
  uniq <- newUnique
  case lookupVar g l n of
      Just(_,TableVal u _)  -> do
          let newNl = case maybeN of
                Just _ -> "self" : nlFn
                Nothing -> nlFn
          let fn = luaFunc (FuncBody (ParamList (NameList newNl) varArg) b)
          updateTableVal g l (u, vl) (FuncVal uniq (length newNl) fn)
          return (g,l,Nothing)
      Just(env, v) -> do
          if not $ null vl then error $ "tried to index a non table value: " ++ show v
            else do
              let fn = luaFunc (FuncBody (ParamList (NameList nlFn) varArg) b)
              let funcV = FuncVal uniq (length nlFn) fn
              case env of
                  Left gEnv -> do
                      let g2 = insertVarGlobal g n funcV
                      return (g2,l,Nothing)
                  Right _ -> do
                      let (g2,l2) = insertVarLocal g l n funcV
                      return (g2,l2,Nothing)
      Nothing -> do
          if not $ null vl then error $ "tried to index a non table value: " ++ show NilVal
            else do
              let fn = luaFunc (FuncBody (ParamList (NameList nlFn) varArg) b)
              let g2 = insertVarGlobal g n (FuncVal uniq (length nlFn) fn)
              return (g2,l,Nothing)



interpretS g l (LocalFuncStat n fb) = do
  let fn = luaFunc fb
  let (FuncBody (ParamList (NameList nl) _) _) = fb
  let numArgs = length nl
  uniq <- newUnique
  let (g2, l2) = insertVarCurrent g l n (FuncVal uniq numArgs fn)
  return (g2, l2, Nothing)
interpretS _ _ Break = error "Internal error: a break should be handled in statement list processing"
interpretS _ _ (Label _) = error "Internal error: a label should be handled in statement list processing"
interpretS _ _ (Goto _) = error "Internal error: a goto should be handled in statement list processing"

-- | Regular for loop iteration
forIter :: GlobalEnv -> Env -> Name -> Val -> Val -> Block -> IO (GlobalEnv, Env, Maybe [Val])
forIter g l n (NumVal end) (NumVal incr) b
  -- end condition
  | current > end = return (g, l, Nothing)
  | otherwise = do
      -- create loop environment
      let loopEnv = newLocalEnv (Just l)
      -- interpret block
      (g2, loopEnv2, ret) <- interpretB g loopEnv b
      -- get updated parent env
      let l2 = case getParent loopEnv2 of
            Just x -> x
            Nothing -> error "Internal error"
      case ret of
        Nothing -> do
          -- update index variable and keep going
          let (g3, l3) = insertVarLocal g2 l2 n (NumVal $ current + incr)
          forIter g3 l3 n (NumVal end) (NumVal incr) b
        -- exit loop in case of return statement
        Just _ -> return (g2, l2, ret)
  where
    -- Gets the current index from the local environment
    -- Note that we can use getVal here, because we know it exists within the environment.
    (NumVal current) = getVar g l n

-- | Template for creating a lua function at runtime
luaFunc :: FuncBody -> LuaFunc
luaFunc (FuncBody (ParamList (NameList nl) varArg) b) g l vl = do
  let paramLen = length nl
  let funcEnv = newLocalEnv (Just l)
  let (g2, funcEnv2) = insertVarsCurrent g funcEnv nl (take paramLen vl)
  let funcEnv3 = case varArg of
        Just _ -> insertVarArgs funcEnv2 $ drop paramLen vl
        Nothing -> funcEnv2
  (g3, funcEnv4, retvals) <- interpretB g2 funcEnv3 b
  let finalVL = fromMaybe [VoidVal] retvals
  let l2 = fromMaybe EnvEmpty $ getParent funcEnv4
  return (g3, l2, finalVL)
