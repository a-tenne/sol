module Runtime.Runtime where

import AST
import Control.Monad (replicateM)
import Data.IORef (IORef, newIORef, readIORef)
import Data.List (find, intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Text.ICU as ICU
import Data.Time
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), ZonedTime (..), getCurrentTimeZone, localTimeToUTC)
import Data.Unique (Unique, newUnique)
import GHC.IO.Exception
import GHC.IO.Handle.Internals (wantReadableHandle)
import GHC.IORef (writeIORef)
import Parser.Parser (num)
import Runtime.Types
import System.CPUTime (getCPUTime)
import System.Directory
import System.Environment
import System.Exit
import System.IO.Error
import System.Info (os)
import System.Posix
import System.Process (system)
import Text.Parsec (parse)

-- | Turns an AST literal into a runtime value.
--  Note that int64 values are turned into float64.
--  This might get changed in the future, since it does not comply with the Lua standard.
valFromLiteral :: Literal -> Val
valFromLiteral (StringLit x) = StringVal x
valFromLiteral (NumLit (NumDouble x)) = NumVal x
valFromLiteral (NumLit (NumInt x)) = NumVal $ fromIntegral x

-- | Checks if a value is truthy.
--  All values are truthy except for nil and false.
valIsTrue :: Val -> Bool
valIsTrue (BoolVal False) = False
valIsTrue NilVal = False
valIsTrue _ = True

-- | Coerces a string to a number value. Calls the num parser from the frontend to do so.
coerce :: String -> Val
coerce str = case parse num "" str of
  Left _ -> error $ "attempt to coerce string " ++ show str ++ " to a number"
  Right (LiteralExpr x) -> valFromLiteral x

-- | Initializes the global environment variables.
initialEnvVars :: IO (M.Map String Val)
initialEnvVars = do
  printName <- newUnique
  osTable <- osEntry
  return $ M.fromList [("print", FuncVal printName 1 luaPrint), ("os", osTable)]

-- | Initializes the global environment.
initialEnv :: IO GlobalEnv
initialEnv = do
  v <- initialEnvVars
  return GlobalEnv {vars = v, collator = ICU.collator ICU.Current}

-- | Inserts a variable strictly into the global environment.
insertVarGlobal :: GlobalEnv -> String -> Val -> GlobalEnv
insertVarGlobal current key value = GlobalEnv {vars = M.insert key value (vars current), collator = collator current}

-- | Inserts a list of variables strictly into the global environment.
insertVarsGlobal :: GlobalEnv -> [String] -> [Val] -> GlobalEnv
insertVarsGlobal current [] _ = current
insertVarsGlobal current _ [] = current
insertVarsGlobal current (x : xs) (y : ys) = insertVarsGlobal (insertVarGlobal current x y) xs ys

-- | Checks wether a variable exists under the given identifier in the global environment.
lookupVarGlobal :: GlobalEnv -> String -> Maybe Val
lookupVarGlobal g key = M.lookup key (vars g)

-- | Checks wether a variable exists under the given identifier in the local environment.
lookupVarLocal :: Env -> String -> Maybe Val
lookupVarLocal EnvEmpty _ = Nothing
lookupVarLocal (Env localVars _ parent) key = case M.lookup key localVars of
  Nothing -> lookupVarLocal parent key
  x -> x

-- | Checks wether a variable exists under the given identifier in the global and local environment.
--  If it does, we return a tuple with the correct environment and the value of that variable.
lookupVar :: GlobalEnv -> Env -> String -> Maybe (Either GlobalEnv Env, Val)
lookupVar g l key = case resL of
  Just x -> Just (Right l, x)
  Nothing -> case resG of
    Just y -> Just (Left g, y)
    Nothing -> Nothing
  where
    resG = lookupVarGlobal g key
    resL = lookupVarLocal l key

-- | Predicate function to check if a table has the same unique identifier.
predicate :: Unique -> Val -> Bool
predicate uniq (TableVal u _) = u == uniq
predicate _ _ = False

-- | Searches all environments for the variable name of the table with the given unique identifier.
nameFromUnique :: GlobalEnv -> Env -> Unique -> Maybe String
nameFromUnique g EnvEmpty uniq = fst <$> find (predicate uniq . snd) (M.toList $ vars g)
nameFromUnique g (Env varList _ parent) uniq = case fst <$> find (predicate uniq . snd) (M.toList varList) of
  Nothing -> nameFromUnique g parent uniq
  x -> x

-- | Looks up the table with it's unique identifier, then descends up until the value that has to be changed and mutates it.
updateTableVal :: GlobalEnv -> Env -> (Unique, [Val]) -> Val -> IO ()
updateTableVal g l (uniq, vl) newV = case nameFromUnique g l uniq of
  Nothing -> return ()
  -- Expects the value to be a table, mutates it and inserts it into the scope where it belongs
  Just n -> case getVar g l n of
    (TableVal u t) -> mutateTableVal t vl newV
    _ -> error "internal error"
  where
    mutateTableVal :: IORef Table -> [Val] -> Val -> IO ()
    -- Base case: the list only has one index value left, so we update the table with it
    mutateTableVal tRef [v] newV = do
      t <- readIORef tRef
      let tNew = tableInsert t v newV
      writeIORef tRef tNew
    -- We check if the key exists within the given table. If the key does not exist or is not a table
    -- we throw an error. If it is, we call mutateTableVal on that table.
    mutateTableVal tRef (v : vs) newV = do
      (Table m) <- readIORef tRef
      case M.lookup v m of
        Just (TableVal _ tRef2) -> do
          mutateTableVal tRef2 vs newV
        _ -> error "internal error"
    -- The value list can never be empty.
    mutateTableVal _ [] _ = error "internal error"

-- | Tries finding a variable by name in all of the environments. If it doesn't, it returns nil.
getVar :: GlobalEnv -> Env -> String -> Val
getVar g l key = case resL of
  Just x -> x
  Nothing -> fromMaybe NilVal resG
  where
    resG = lookupVarGlobal g key
    resL = lookupVarLocal l key

-- | Creates a new local environment, with it's upper scope as the parent environment.
newLocalEnv :: Env -> Env
newLocalEnv = Env M.empty Nothing

-- | Returns the environment's parent, if there is one.
getParent :: Env -> Maybe Env
getParent EnvEmpty = Nothing
getParent (Env _ _ parent) = Just parent

-- | Checks if a var exists locally first. If it exists in any local environment, it inserts it there. If not, it inserts it into the global environment.
insertVarLocal :: GlobalEnv -> Env -> String -> Val -> (GlobalEnv, Env)
insertVarLocal g EnvEmpty key value = (insertVarGlobal g key value, EnvEmpty)
insertVarLocal g (Env localVars varArgs parent) key value = case exists of
  Just _ -> case existsLocally of
    Just _ -> (g, Env (M.insert key value localVars) varArgs parent)
    Nothing -> let (g2, l2) = insertVarLocal g parent key value in (g2, Env localVars varArgs l2)
  Nothing -> (g, Env (M.insert key value localVars) varArgs parent)
  where
    existsLocally = M.lookup key localVars
    exists = lookupVar g (Env localVars varArgs parent) key

-- | Calls insertVarLocal for a list of variables.
insertVarsLocal :: GlobalEnv -> Env -> [String] -> [Val] -> (GlobalEnv, Env)
insertVarsLocal g current [] _ = (g, current)
insertVarsLocal g current _ [] = (g, current)
insertVarsLocal g current (x : xs) (y : ys) = insertVarsLocal g2 l2 xs ys
  where
    (g2, l2) = insertVarLocal g current x y

-- | Insert a variable list as varargs into the given local environment.
insertVarArgs :: Env -> [Val] -> Env
insertVarArgs EnvEmpty _ = error "Internal error: cannot have var args in global scope"
insertVarArgs (Env localVars Nothing parent) newVars = Env localVars (Just newVars) parent
insertVarArgs (Env localVars (Just varArgs) parent) newVars = Env localVars (Just $ varArgs ++ newVars) parent

-- | Returns varargs of the given local environment.
getVarArgs :: Env -> Maybe [Val]
getVarArgs EnvEmpty = Nothing
getVarArgs (Env _ varArgs _) = varArgs

-- | Force inserts a variable into the current scope.
insertVarCurrent :: GlobalEnv -> Env -> String -> Val -> (GlobalEnv, Env)
insertVarCurrent g EnvEmpty key value = (insertVarGlobal g key value, EnvEmpty)
insertVarCurrent g (Env localVars varArgs parent) key value = (g, Env (M.insert key value localVars) varArgs parent)

-- | Force inserts a list of variables into the current scope.
insertVarsCurrent :: GlobalEnv -> Env -> [String] -> [Val] -> (GlobalEnv, Env)
insertVarsCurrent g l [] _ = (g, l)
insertVarsCurrent g l _ [] = (g, l)
insertVarsCurrent g l (x : xs) (y : ys) = insertVarsCurrent g2 l2 xs ys
  where
    (g2, l2) = insertVarCurrent g l x y

-- | Looks up a table value with the given key.
tableLookup :: Table -> Val -> Val
tableLookup (Table m) key = fromMaybe NilVal (M.lookup key m)

-- | Inserts a table value with its key.
tableInsert :: Table -> Val -> Val -> Table
tableInsert (Table m) key value = Table $ M.insert key value m

-- | Cuts off the last void value from a list of values, replaces the other ones with nil.
--  Needed for luaPrint, for example.
formatVals :: [Val] -> [Val]
formatVals [] = []
formatVals (x : xs)
  | x == VoidVal && null xs = []
  | x == VoidVal = NilVal : formatVals xs
  | otherwise = x : formatVals xs

-- | Turns all void values in a list of values into nil.
cleanVals :: [Val] -> [Val]
cleanVals [] = []
cleanVals (x : xs)
  | x == VoidVal = NilVal : formatVals xs
  | otherwise = x : formatVals xs

-- | Helper function to print an error if there was an unexpected argument.
badArgument :: String -> Val -> IO (GlobalEnv, Env, [Val])
badArgument expectedName actual =
  error $ "Bad argument " ++ show actual ++ ", expected " ++ expectedName

-- | Helper function to print an error if a function has yet to be implemented.
funcNotImplemented :: String -> LuaFunc
funcNotImplemented err = error $ "sol does not yet implement the function " ++ err

-- | Lua's native print function.
luaPrint :: LuaFunc
luaPrint g l args = do
  let newArgs = formatVals args
  putStrLn $ intercalate "\t" $ map show newArgs
  return (g, l, [VoidVal])

-- | Returns the standard library's os table.
osEntry :: IO Val
osEntry = do
  uids <- replicateM 12 newUnique
  let [ tUid,
        clockUid,
        dateUid,
        difftimeUid,
        executeUid,
        exitUid,
        getenvUid,
        removeUid,
        renameUid,
        setlocaleUid,
        timeUid,
        tmpnameUid
        ] = uids
  t <-
    newIORef
      ( Table $
          M.fromList
            [ (StringVal "clock", FuncVal clockUid 0 osClock),
              (StringVal "date", FuncVal timeUid 2 osDate),
              (StringVal "difftime", FuncVal timeUid 2 osDifftime),
              (StringVal "execute", FuncVal timeUid 1 osExecute),
              (StringVal "exit", FuncVal timeUid 2 osExit),
              (StringVal "getenv", FuncVal timeUid 1 osGetenv),
              (StringVal "remove", FuncVal timeUid 1 osRemove),
              (StringVal "rename", FuncVal timeUid 2 osRename),
              (StringVal "setlocale", FuncVal timeUid 2 osSetlocale),
              (StringVal "time", FuncVal timeUid 1 osTime),
              (StringVal "tmpname", FuncVal timeUid 0 osTmpname)
            ]
      )
  let osTable = TableVal tUid t
  return osTable

-- | Lua's native os.clock function.
osClock :: LuaFunc
osClock g l args = do
  time <- getCPUTime
  let timeSeconds = fromIntegral time * (10 ^ (-12))
  return (g, l, [NumVal timeSeconds])

-- | Lua's native os.date function.
-- The current version of the interpreter does not implement this function.
-- Calling it will result in a runtime error.
osDate :: LuaFunc
osDate = funcNotImplemented "os.date"

-- | Lua's native os.difftime function.
-- The current version of the interpreter does not implement this function.
-- Calling it will result in a runtime error.
osDifftime :: LuaFunc
osDifftime = funcNotImplemented "os.date"

-- | Lua's native os.execute function.
osExecute :: LuaFunc
osExecute g l [NilVal] = do
  sh <- lookupEnv "SHELL"
  if isJust sh
    then return (g, l, [BoolVal True])
    else return (g, l, [NilVal])
osExecute g l [StringVal cmd] = do
  mShell <- lookupEnv "SHELL"
  case mShell of
    Nothing -> return (g, l, [BoolVal False])
    Just shell -> do
      pid <- forkProcess $ executeFile shell True ["-c", cmd] Nothing
      status <- getProcessStatus True False pid
      case status of
        Just (Exited ExitSuccess) ->
          return (g, l, [BoolVal True, StringVal "exit", NumVal 0])
        Just (Exited (ExitFailure code)) ->
          return (g, l, [NilVal, StringVal "exit", NumVal (fromIntegral code)])
        Just (Terminated sig _) ->
          return (g, l, [NilVal, StringVal "signal", NumVal (fromIntegral sig)])
        Just (Stopped sig) ->
          return (g, l, [NilVal, StringVal "signal", NumVal (fromIntegral sig)])
        Nothing ->
          return (g, l, [NilVal])

-- | Lua's native os.exit function.
osExit :: LuaFunc
-- Default is true
osExit _ _ [NilVal] = exitSuccess
osExit _ _ [BoolVal True] = exitSuccess
osExit _ _ [BoolVal False] = exitFailure
osExit _ _ [NumVal code] = exitWith (ExitFailure (floor code))
osExit _ _ [_, BoolVal _] = exitSuccess
osExit _ _ (x : _) = badArgument "boolean or number" x

-- | Lua's native os.getenv function.
osGetenv :: LuaFunc
osGetenv g l [StringVal var] = do
  val <- lookupEnv var
  case val of
    Just v -> return (g, l, [StringVal v])
    Nothing -> return (g, l, [NilVal])
osGetenv _ _ (x : _) = badArgument "string" x

-- | Lua's native os.remove function.
-- The current version of the interpreter does not implement this function.
-- Calling it will result in a runtime error.
osRemove :: LuaFunc
osRemove = funcNotImplemented "os.remove"

-- | Lua's native os.rename function.
-- The current version of the interpreter does not implement this function.
-- Calling it will result in a runtime error.
osRename :: LuaFunc
osRename = funcNotImplemented "os.rename"

-- | Lua's native os.setlocale function.
-- The current version of the interpreter does not implement this function.
-- Calling it will result in a runtime error.
osSetlocale :: LuaFunc
osSetlocale = funcNotImplemented "os.setlocale"

-- | Lua's native os.time function.
osTime :: LuaFunc
osTime g l [NilVal] = do
  now <- getPOSIXTime
  return (g, l, [NumVal (realToFrac (round now :: Integer))])
osTime g l (x : _) = do
  case x of
    (TableVal uid tRef) -> do
      t <- readIORef tRef
      let (NumVal year) = case tableLookup t (StringVal "year") of
            NilVal -> error "date table is missing the year field"
            (NumVal a) -> NumVal a
            (StringVal a) -> coerce a
            _ -> error "year field is not an integer"
          (NumVal month) = case tableLookup t (StringVal "month") of
            NilVal -> error "date table is missing the month field"
            (NumVal a) -> NumVal a
            (StringVal a) -> coerce a
            _ -> error "month field is not an integer"
          (NumVal day) = case tableLookup t (StringVal "day") of
            NilVal -> error "date table is missing the day field"
            (NumVal a) -> NumVal a
            (StringVal a) -> coerce a
            _ -> error "day field is not an integer"
          (NumVal hour) = case tableLookup t (StringVal "hour") of
            NilVal -> NumVal 12
            (NumVal a) -> NumVal a
            (StringVal a) -> coerce a
            _ -> error "hour field is not an integer"
          (NumVal min) = case tableLookup t (StringVal "min") of
            NilVal -> NumVal 0
            (NumVal a) -> NumVal a
            (StringVal a) -> coerce a
            _ -> error "min field is not an integer"
          (NumVal sec) = case tableLookup t (StringVal "sec") of
            NilVal -> NumVal 0
            (NumVal a) -> NumVal a
            (StringVal a) -> coerce a
            _ -> error "sec field is not an integer"
      case tableLookup t (StringVal "isdst") of
        NilVal -> return ()
        _ -> print "sol does not yet support isdst as an argument"

      tz <- getCurrentTimeZone
      let day' = fromGregorian (floor year) (floor month) (floor day)
          tod = TimeOfDay (floor hour) (floor min) (realToFrac sec)
          lt = LocalTime day' tod
          utc = localTimeToUTC tz lt
          posix = floor $ utcTimeToPOSIXSeconds utc

      return (g, l, [NumVal (fromIntegral posix)])
    _ -> badArgument "table" x

-- | Lua's native os.tmpname function.
-- The current version of the interpreter does not implement this function.
-- Calling it will result in a runtime error.
osTmpname :: LuaFunc
osTmpname = funcNotImplemented "os.tmpname"
