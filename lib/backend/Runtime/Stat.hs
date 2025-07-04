module Runtime.Stat where

import AST
import Control.Monad (void)
import Runtime.Expr
import Runtime.Runtime
import Runtime.Types
import Data.Bits (Bits(xor))
import Data.Unique (Unique, newUnique)
import Data.Maybe (fromMaybe)

interpret :: AST -> IO ()
interpret (AST ch) = interpretCh ch

interpretCh :: Chunk -> IO ()
interpretCh (Chunk b) = do
  g <- initialEnv
  void $ interpretB g EnvEmpty b

interpretB :: GlobalEnv -> Env -> Block -> IO (GlobalEnv, Env, Maybe [Val])
interpretB g l (Block sl rs) = do
  (g2,l2,ret) <- interpretSL g l sl
  case ret of
    -- Case: statement list returned, stop here
    Just _ -> return (g2, l2,ret)
    -- Case: statement list didn't return, check block's return
    Nothing -> case rs of
      -- Case: there is a return. Interpret EL and return values
      Just (RetStat el) -> do
        (g3,l3,vl) <- interpretEL g2 l2 el
        return (g3, l3, Just vl)
      -- Case: We have no return whatsoever
      Nothing -> return (g2,l2,Nothing)

-- Take in the global and local environments, process that list of statements.
-- If there is a "return" statement at any point, and only then, the last element
-- of the tuple becomes "Just [Val]" and at that point, the stack must be unwinded
interpretSL :: GlobalEnv -> Env -> StatList -> IO (GlobalEnv, Env, Maybe [Val])
interpretSL g l (AST.StatList []) = return (g, l, Nothing)
-- Returns VoidVal so the stack unwinds.
interpretSL g l (AST.StatList (Break:_)) = return (g,l,Just [VoidVal])
-- Discards the rest of the SL, SL in label becomes new SL
interpretSL g l (AST.StatList ((Goto n):_)) = do
  let v = lookupVar g l n
  case v of
    Just x -> case x of
        (Left g2,v2) -> if l /= EnvEmpty then labelError else
          case v2 of
            (LabelVal sl) -> interpretSL g2 l sl
            _ -> labelError
        (Right l2, v2) -> if l /= l2 then labelError else
          case v2 of
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
    Just _ -> return (g2,l2, ret)

interpretS :: GlobalEnv -> Env -> Stat -> IO (GlobalEnv, Env, Maybe[Val])
-- Semicolons are NO-OP
interpretS g l Semic = return (g, l, Nothing)
-- Global assignment (or general reassignment) of vars
interpretS g l (Asgn (VarList varl) el) = do
  (g2,l2,vl) <- interpretEL g l el
  -- In case there's void values here, turn them into nil
  let cleanVl = cleanVals vl
  (g3,l3,finalVarL) <- processVarL g2 l2 varl
  let (g4,l4) = updateValues g3 l3 finalVarL cleanVl
  return (g4,l4,Nothing)
  where
    processVariable :: GlobalEnv -> Env -> Var -> IO (GlobalEnv, Env, Either Name (Unique ,[Val]))
    processVariable g l (Var (PrefixName n PrefixEmpty)) = return (g,l, Left n)
    processVariable g l (Var x) = do
      (g2,l2, uid, _) <- interpretPE g l x
      case uid of
          Just x -> return (g2,l2,Right x)
          Nothing -> error "internal error"
    processVarL :: GlobalEnv -> Env -> [Var] -> IO(GlobalEnv, Env, [Either Name (Unique, [Val])])
    processVarL g l [v] = do
      (g2,l2, v2) <- processVariable g l v
      return (g2,l2,[v2])
    processVarL g l (v:vs) = do
      (g2,l2, v2) <- processVariable g l v
      (g3,l3, vTail) <- processVarL g2 l2 vs
      return (g3,l3, v2 : vTail)
    processVarL g l [] = error "internal error"
    updateValue :: GlobalEnv -> Env -> Either Name (Unique, [Val]) -> Val -> (GlobalEnv, Env)
    updateValue g l (Left n) v = insertVarLocal g l n v
    updateValue g l (Right uid) v = updateTableVal g l uid v
    updateValues :: GlobalEnv -> Env -> [Either Name (Unique, [Val])] -> [Val] -> (GlobalEnv, Env)
    updateValues g l [] [] = (g,l)
    updateValues g l [] _ = (g,l)
    updateValues g l (x:xs) [] = let (g2,l2) = updateValue g l x NilVal in updateValues g2 l2 xs []
    updateValues g l (x:xs) (y:ys) = let (g2,l2) = updateValue g l x y in updateValues g2 l2 xs ys

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
      let (g3,l3) = insertVarsCurrent g2 l2 nl vl2
      return (g3,l3,Nothing)
    else do
      let (g3,l3) = insertVarsCurrent g2 l2 nl vl
      return (g3,l3,Nothing)
interpretS g l (FuncCallStat (FuncCall ex)) = do
  (g2, l2, _, _) <- interpretPE g l ex
  return (g2, l2, Nothing)
interpretS g l (ForStat n e1 e2 me b) = do
  (g2,l2, start:_) <- interpretE g l e1
  (g3,l3, end:_) <- interpretE g2 l2 e2
  (g4,l4, incr:_) <- case me of
    Just x -> interpretE g3 l3 x
    Nothing -> return (g3,l3, [NumVal 1])
  assertNumber start >> assertNumber end >> assertNumber incr
  -- insert index into env
  let (g5,l5) = insertVarCurrent g4 (newLocalEnv (Just l4)) n start
  -- run loop
  -- Note: the loop creates an own environment on each iteration
  (g6,l6, ret) <- forIter g5 l5 n end incr b
  -- gets updated parent env
  let l7 = case getParent l6 of
        Just x -> x
        Nothing -> error "Internal error"
  -- return updated local env without index and new global env
  return (g6,l7,ret)
    where
      assertNumber :: Val -> IO()
      assertNumber (NumVal _) = return ()
      assertNumber x = error $ "for loop index value " ++ show x ++ " is not a number"

interpretS g l (WhileDo e b) = do
  (g2,l2,v:_) <- interpretE g l e
  if valIsTrue v
    then do
      let loopEnv = newLocalEnv (Just l2)
      (g3, loopEnv2, ret) <- interpretB g2 loopEnv b
      let l3 = case getParent loopEnv2 of
            Just x -> x
            Nothing -> error "Internal error"
      case ret of
          Just _ -> return (g3,l3,ret)
          Nothing -> interpretS g3 l3 (WhileDo e b)
    else
      return (g2,l2,Nothing)

interpretS g l (RepeatUntil b e) = do
  let loopEnv = newLocalEnv (Just l)
  (g2, loopEnv2, ret) <- interpretB g loopEnv b
  let l2 = case getParent loopEnv2 of
        Just x -> x
        Nothing -> error "Internal error"
  case ret of
      Just _ -> return (g2,l2,ret)
      Nothing -> do
        (g3,l3, v:_) <- interpretE g2 l2 e
        if valIsTrue v
          then return (g,l,Nothing)
          else interpretS g3 l3 (RepeatUntil b e)

interpretS _ _ (IfStat {}) = undefined
interpretS _ _ (ForIn {}) = undefined
interpretS _ _ (FuncDefStat {}) = undefined
interpretS g l (LocalFuncStat n fb) = do
  let fn = luaFunc fb
  let (FuncBody (ParamList (NameList nl)_)_) = fb
  let numArgs = length nl
  uniq <- newUnique
  let (g2,l2) = insertVarCurrent g l n (FuncVal uniq numArgs fn) 
  return (g2,l2,Nothing)


interpretS _ _ Break = error "Internal error: a break should be handled in statement list processing"
interpretS _ _ (Label _) = error "Internal error: a label should be handled in statement list processing"
interpretS _ _ (Goto _) = error "Internal error: a goto should be handled in statement list processing"
-- Regular for loop iteration.
forIter :: GlobalEnv -> Env -> Name -> Val -> Val -> Block -> IO(GlobalEnv, Env, Maybe[Val])
forIter g l n (NumVal end) (NumVal incr) b
  -- end condition
  | current > end = return (g,l, Nothing)
  | otherwise = do
    -- create loop environment
    let loopEnv = newLocalEnv (Just l)
    -- interpret block
    (g2,loopEnv2, ret) <- interpretB g loopEnv b
    -- get updated parent env
    let l2 = case getParent loopEnv2 of
          Just x -> x
          Nothing -> error "Internal error"
    case ret of
      Nothing -> do
        -- update index variable and keep going
        let (g3,l3) = insertVarLocal g2 l2 n (NumVal $ current + incr)
        forIter g3 l3 n (NumVal end) (NumVal incr) b
      -- exit loop in case of return statement
      Just _ -> return (g2,l2, ret)
      where
        -- Gets the current index from the local environment
        -- Note that we can use getVal here, because we know it exists within the environment.
        (NumVal current) = getVar g l n

luaFunc :: FuncBody -> LuaFunc
luaFunc (FuncBody (ParamList (NameList nl) varArg) b) g l vl = do
  let paramLen = length nl
  let isVarArg = case varArg of
        Just _ -> True
        Nothing -> False
  let funcEnv = newLocalEnv (Just l)
  let (g2, funcEnv2) = insertVarsCurrent g funcEnv nl (take paramLen vl)
  let funcEnv3 = insertVarArgs funcEnv2 $ drop (length vl - paramLen) vl
  (g3, funcEnv4, retvals) <- interpretB g2 funcEnv3 b
  let finalVL = fromMaybe [VoidVal] retvals
  let l2 = fromMaybe EnvEmpty $ getParent funcEnv4
  return (g3, l2, finalVL)
