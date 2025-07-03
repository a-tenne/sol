module Runtime.Stat where

import AST
import Control.Monad (void)
import Runtime.Expr
import Runtime.Runtime
import Runtime.Types

interpret :: AST -> IO ()
interpret (AST ch) = interpretCh ch

interpretCh :: Chunk -> IO ()
interpretCh (Chunk b) = void $ interpretB initialEnv EnvEmpty b

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
-- Discards the rest of the SL
interpretSL g l (AST.StatList (Break:_)) = return (g,l,Nothing)
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
-- Global assignment
interpretS g l (Asgn (VarList vl) el) = undefined
interpretS g l (LocalAsgn (AttrNameList attrnL) mEl) = do
  let (nl, _) = unzip attrnL -- Note: attributes don't do anything yet
  (g2, l2, vl) <- case mEl of
    Nothing -> return (g, l, [NilVal])
    Just x -> interpretEL g l x
  let lenDiff = length nl - length vl
  if lenDiff > 0
    then do
      let vl2 = vl ++ replicate lenDiff NilVal
      let (g3,l3) = insertVarsLocal g2 l2 nl vl2
      return (g3,l3,Nothing)
    else do
      let (g3,l3) = insertVarsLocal g2 l2 nl vl
      return (g3,l3,Nothing)
interpretS g l (FuncCallStat (FuncCall ex)) = do
  (g2, l2, _) <- interpretPE g l ex
  return (g2, l2, Nothing)
interpretS g l (ForStat n e1 e2 me b) = do
  (g2,l2, start:_) <- interpretE g l e1
  (g3,l3, end:_) <- interpretE g2 l2 e2
  (g4,l4, incr:_) <- case me of
    Just x -> interpretE g3 l3 x
    Nothing -> return (g3,l3, [NumVal 1])
  assertNumber start >> assertNumber end >> assertNumber incr
  -- create environment for the index variable
  -- insert index into indexEnv
  let (g5,indexEnv) = insertVarCurrent g4 (newLocalEnv (Just l4)) n start
  -- create new environment for loop with indexEnv as parent
  let loopEnv = newLocalEnv (Just indexEnv)
  -- run loop with loop environment
  (g6, ret) <- forIter g5 loopEnv n end incr b
  -- return old local env and new global env
  return (g6,l4,ret)
    where
      assertNumber :: Val -> IO()
      assertNumber (NumVal _) = return ()
      assertNumber x = error $ "for loop index value " ++ show x ++ " is not a number"
interpretS _ _ (WhileDo {}) = undefined
interpretS _ _ (RepeatUntil {}) = undefined
interpretS _ _ (IfStat {}) = undefined
interpretS _ _ (ForIn {}) = undefined
interpretS _ _ (FuncDefStat {}) = undefined
interpretS _ _ (LocalFuncStat {}) = undefined
interpretS _ _ Break = error "Internal error: a break should be handled in statement list processing"
interpretS _ _ (Label _) = error "Internal error: a label should be handled in statement list processing"
interpretS _ _ (Goto _) = error "Internal error: a goto should be handled in statement list processing"
-- Regular for loop iteration. Does not return a local environment, since its own gets discarded
forIter :: GlobalEnv -> Env -> Name -> Val -> Val -> Block -> IO(GlobalEnv, Maybe[Val])
forIter g l n (NumVal end) (NumVal incr) b
  -- end condition
  | current > end = return (g, Nothing)
  | otherwise = do
    -- interpret block
    (g2,l2, ret) <- interpretB g l b
    case ret of
      Nothing -> do
        -- update index variable and keep going
        let (g3,l3) = insertVarLocal g l n (NumVal $ current + incr)
        forIter g3 l3 n (NumVal end) (NumVal incr) b
      -- exit loop in case of return statement
      Just _ -> return (g2, ret)
      where
        (Env _ parent) = l
        -- Gets the current index from the upper local environment
        -- Note that we can use getVal here, because we know it exists within the environment.
        (NumVal current) = getVar g parent n
