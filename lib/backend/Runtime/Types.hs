module Runtime.Types where

import Data.Map(Map)
import Data.Char (toLower)
import Data.Text.ICU (Collator)
import AST


data Val = StringVal String | NumVal Double | BoolVal Bool | NilVal | VoidVal
  deriving (Eq)

type NativeFunc = GlobalEnv -> Env -> [Val] -> IO(GlobalEnv, Env, [Val])

instance Show Val where
  show (StringVal x) = x
  show (NumVal x) = if fromIntegral (round x) == x then show $ round x else show x
  show (BoolVal x) = map toLower $ show x
  show NilVal = "nil"
  show VoidVal = ""

data GlobalEnv = GlobalEnv { vars :: Map String Val, collator :: Collator }

instance Show GlobalEnv where
  show x = "GlobalEnv {" ++ show (vars x) ++ "}"

instance Eq GlobalEnv where
  x == y = vars x == vars y

data Env = Env (Map String Val) Env | EnvEmpty
  deriving(Eq, Show)

