module Runtime.Stat where
import Runtime.Types
import Runtime.Runtime
import Runtime.Expr
import AST
import Control.Monad (void)

interpret :: AST -> IO()
interpret (AST ch) = interpretCh ch

interpretCh :: Chunk -> IO()
interpretCh (Chunk b) = void $ interpretB initialEnv EnvEmpty b

interpretB :: GlobalEnv -> Env -> Block -> IO(GlobalEnv, Env, [Val])
interpretB g l (Block sl rs) = do
  (g2, l2) <- interpretSL g l sl
  if l2 == EnvEmpty
    then
      return (g2, l2, [])
    else do
      (g3, l3, vl) <- case rs of
          Nothing -> return (g2,l2,[])
          Just (RetStat el) -> interpretEL g2 l2 el
      return (g3, l3, vl)


interpretSL :: GlobalEnv -> Env -> StatList -> IO(GlobalEnv, Env)
interpretSL = undefined

interpretS :: GlobalEnv -> Env -> Stat -> IO(GlobalEnv, Env)
interpretS g l Semic = return (g,l)
interpretS g l (Asgn (VarList []) (ExprList [])) = return (g,l)
interpretS g l (LocalAsgn (AttrNameList []) _) = return (g,l)
interpretS g l (LocalAsgn (AttrNameList ((n, _):as)) mEl) = do -- Note: attributes don't do anything yet
  (g2, l2, v) <- case mEl of
      Nothing -> return (g, l, NilVal)
      Just (ExprList []) -> return (g, l, NilVal)
      Just (ExprList (x:_)) -> interpretE g l x
  let newMEl = case mEl of
        Nothing -> Nothing
        Just (ExprList []) -> Nothing
        Just (ExprList (x:xs)) -> Just $ ExprList xs
  if l == EnvEmpty
    then do
      let g3 = insertVarGlobal g2 n v
      interpretS g3 l2 (LocalAsgn (AttrNameList as) newMEl)
    else do
      let l3 = insertVarLocal l n v
      interpretS g2 l3 (LocalAsgn (AttrNameList as) newMEl)
interpretS _ _ Break = error "Internal error: a break should be handled in statement list processing"
interpretS _ _ (Label _) = error "Internal error: a label should be handled in statement list processing"
interpretS _ _ (Goto _) = error "Internal error: a goto should be handled in statement list processing"
