module Runtime.Runtime where
import Runtime.Types
import Runtime.Expr
import qualified Data.Text.ICU as ICU
import Data.Map (empty, insert)
import Data.List (intercalate)

initialEnv = GlobalEnv {vars = empty, collator = ICU.collator ICU.Current}

insertVarGlobal :: GlobalEnv -> String -> Val -> GlobalEnv
insertVarGlobal current key value  = GlobalEnv {vars = insert key value (vars current), collator = collator current}

newLocalEnv :: Maybe Env -> Env
newLocalEnv Nothing = Env empty EnvEmpty
newLocalEnv (Just parent) = Env empty parent

insertVarLocal :: Env -> String -> Val -> Env
insertVarLocal EnvEmpty  _ _ = EnvEmpty
insertVarLocal (Env localVars parent)key value  = Env (insert key value localVars) parent

luaPrint :: NativeFunc
luaPrint g l args = do
  if null args
    then
      error "function \"print\" needs at least one argument"
    else do
      putStrLn $ intercalate "\t" (formatVals args)
      return (g,l,[VoidVal])
    where
      formatVals :: [Val] -> [String]
      formatVals [] = []
      formatVals (x:xs)
        | x == VoidVal && null xs = []
        | x == VoidVal = show NilVal : formatVals xs
        | otherwise = show x : formatVals xs
