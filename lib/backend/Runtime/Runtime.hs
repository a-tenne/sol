module Runtime.Runtime where

import Data.List (intercalate, find)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text.ICU as ICU
import Runtime.Types
import Data.Unique (newUnique, Unique)
import GHC.IO.Handle.Internals (wantReadableHandle)

initialEnvVars :: IO (M.Map String Val)
initialEnvVars = do
  printName <- newUnique
  return $ M.fromList [("print", NatFuncVal printName luaPrint)]

initialEnv :: IO GlobalEnv
initialEnv = do
  v <- initialEnvVars
  return GlobalEnv {vars = v, collator = ICU.collator ICU.Current}

insertVarGlobal :: GlobalEnv -> String -> Val -> GlobalEnv
insertVarGlobal current key value = GlobalEnv {vars = M.insert key value (vars current), collator = collator current}

insertVarsGlobal :: GlobalEnv -> [String] -> [Val] -> GlobalEnv
insertVarsGlobal current [] _ = current
insertVarsGlobal current _ [] = current
insertVarsGlobal current (x : xs) (y : ys) = insertVarsGlobal (insertVarGlobal current x y) xs ys

lookupVarGlobal :: GlobalEnv -> String -> Maybe Val
lookupVarGlobal g key = M.lookup key (vars g)

lookupVarLocal :: Env -> String -> Maybe Val
lookupVarLocal EnvEmpty _ = Nothing
lookupVarLocal (Env localVars _ parent) key = case M.lookup key localVars of
  Nothing -> lookupVarLocal parent key
  x -> x

lookupVar :: GlobalEnv -> Env -> String -> Maybe(Either GlobalEnv Env, Val)
lookupVar g l key = case resL of
  Just x -> Just (Right l, x)
  Nothing -> case resG of
      Just y -> Just (Left g, y)
      Nothing -> Nothing
  where
    resG = lookupVarGlobal g key
    resL = lookupVarLocal l key

predicate ::  Unique ->  Val -> Bool
predicate uniq (TableVal u _) = u == uniq
predicate _ _ = False

nameFromUnique :: GlobalEnv -> Env -> Unique -> Maybe String
nameFromUnique g EnvEmpty uniq = fst <$> find (predicate uniq . snd) (M.toList $ vars g)
nameFromUnique g (Env varList _ parent) uniq = case fst <$> find (predicate uniq . snd) (M.toList varList) of
    Nothing -> nameFromUnique g parent uniq
    x -> x

updateTableVal :: GlobalEnv -> Env -> (Unique, [Val]) -> Val -> (GlobalEnv, Env)
updateTableVal g l (uniq, vl) newV = case nameFromUnique g l uniq of
    Nothing -> (g,l)
    -- Expects the value to be a table, mutates it and inserts it into the scope where it belongs
    Just n -> case getVar g l n of
        (TableVal u t) -> insertVarLocal g l n (TableVal u (mutateTableVal t vl newV))
        _ -> error "internal error"
    where
      mutateTableVal :: Table -> [Val] -> Val -> Table
      -- Base case: the list only has one index value left, so we update the table with it
      mutateTableVal t [v] newV = tableInsert t v newV
      -- We check if the key exists within the given table. If the key does not exist or is not a table
      -- we throw an error. If it is, we insert the new, mutated table into that old spot.
      -- since it calls itself recursively within the constructor, it updates the actual value we want
      -- to update and all upper values
      mutateTableVal (Table m) (v:vs) newV = case M.lookup v m of
          Just (TableVal u t) -> tableInsert (Table m) v (TableVal u (mutateTableVal t vs newV))
          _ -> error "internal error"
      -- The value list can never be empty.
      mutateTableVal _ [] _ = error "internal error"

getVar :: GlobalEnv -> Env -> String -> Val
getVar g l key = case resL of
  Just x -> x
  Nothing -> fromMaybe NilVal resG
  where
    resG = lookupVarGlobal g key
    resL = lookupVarLocal l key

newLocalEnv :: Maybe Env -> Env
newLocalEnv Nothing = Env M.empty [] EnvEmpty
newLocalEnv (Just parent) = Env M.empty [] parent

getParent :: Env -> Maybe Env
getParent EnvEmpty = Nothing
getParent (Env _ _ parent) = Just parent

insertVarLocal :: GlobalEnv -> Env -> String -> Val -> (GlobalEnv, Env)
insertVarLocal g EnvEmpty key value = (insertVarGlobal g key value, EnvEmpty)
insertVarLocal g (Env localVars varArgs parent) key value = case exists of
    Just _ -> case existsLocally of
      Just _ -> (g, Env (M.insert key value localVars) varArgs parent)
      Nothing ->let (g2, l2) = insertVarLocal g parent key value in (g2,Env localVars varArgs l2)
    Nothing -> (g, Env (M.insert key value localVars) varArgs parent)
  where
    existsLocally = M.lookup key localVars
    exists = lookupVar g (Env localVars varArgs parent) key

insertVarArgs :: Env -> [Val] -> Env
insertVarArgs EnvEmpty _ = error "Internal error: cannot have var args in global scope"
insertVarArgs (Env localVars varArgs parent) newVars = Env localVars (varArgs ++ newVars) parent

getVarArgs :: Env -> [Val]
getVarArgs EnvEmpty = [VoidVal]
getVarArgs (Env _ varArgs _) = varArgs

insertVarCurrent :: GlobalEnv -> Env -> String -> Val -> (GlobalEnv, Env)
insertVarCurrent g EnvEmpty key value = (insertVarGlobal g key value, EnvEmpty)
insertVarCurrent g (Env localVars varArgs parent) key value = (g, Env (M.insert key value localVars) varArgs parent)
insertVarsCurrent :: GlobalEnv -> Env -> [String] -> [Val] -> (GlobalEnv, Env)
insertVarsCurrent g l [] _ = (g, l)
insertVarsCurrent g l _ [] = (g, l)
insertVarsCurrent g l (x : xs) (y : ys) = insertVarsCurrent g2 l2 xs ys
  where
    (g2, l2) = insertVarCurrent g l x y

insertVarsLocal :: GlobalEnv -> Env -> [String] -> [Val] -> (GlobalEnv, Env)
insertVarsLocal g current [] _ = (g, current)
insertVarsLocal g current _ [] = (g, current)
insertVarsLocal g current (x : xs) (y : ys) = insertVarsLocal g2 l2 xs ys
  where
    (g2, l2) = insertVarLocal g current x y

tableLookup :: Table -> Val -> Val
tableLookup (Table m) key = fromMaybe NilVal (M.lookup key m)

tableInsert :: Table -> Val -> Val -> Table
tableInsert (Table m) key value = Table $ M.insert key value m

formatVals :: [Val] -> [Val]
formatVals [] = []
formatVals (x : xs)
  | x == VoidVal && null xs = []
  | x == VoidVal = NilVal : formatVals xs
  | otherwise = x : formatVals xs
cleanVals :: [Val] -> [Val]
cleanVals [] = []
cleanVals (x : xs)
  | x == VoidVal = NilVal : formatVals xs
  | otherwise = x : formatVals xs

checkFnArgs :: String -> Int -> Int -> Bool -> IO ()
checkFnArgs fnName argsNeeded argsProvided isVarArg
  | not isVarArg && argsNeeded /= argsProvided = error $ "function " ++ fnName ++ " needs exactly " ++ show argsNeeded ++ singleOrPlural ++ show argsProvided ++ " were provided."
  | isVarArg && argsNeeded > argsProvided = error $ "function " ++ fnName ++ " needs atleast " ++ show argsNeeded ++ singleOrPlural ++ show argsProvided ++ " were provided."
  | otherwise = return ()
  where
    singleOrPlural = if argsNeeded == 1 then " argument, " else " arguments, "

luaPrint :: NativeFunc
luaPrint g l args = do
  checkFnArgs "print" 1 (length args) True
  putStrLn $ intercalate "\t" $ map show args
  return (g,l, [VoidVal])
