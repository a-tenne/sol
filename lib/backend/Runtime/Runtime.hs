module Runtime.Runtime where
import Runtime.Types
import Runtime.Expr
import qualified Data.Text.ICU as ICU
import Data.Map (empty, insert)

initialEnv = GlobalEnv {vars = empty, collator = ICU.collator ICU.Current}

insertVarGlobal :: String -> Val -> GlobalEnv -> GlobalEnv
insertVarGlobal key value current = GlobalEnv {vars = insert key value (vars current), collator = collator current}

newLocalEnv :: Maybe Env -> Env
newLocalEnv Nothing = Env empty EnvEmpty
newLocalEnv (Just parent) = Env empty parent

insertVarLocal :: String -> Val -> Env -> Env
insertVarLocal _ _ EnvEmpty = EnvEmpty
insertVarLocal key value (Env localVars parent) = Env (insert key value localVars) parent
