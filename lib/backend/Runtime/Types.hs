{-# LANGUAGE GADTs #-}
module Runtime.Types where

import Data.Map(Map)
import Data.Char (toLower)
import Data.Text.ICU (Collator)
import AST
import GHC.IO (unsafePerformIO)
import GHC.StableName (makeStableName, hashStableName)
import Data.Unique

data Table where
  Table :: (Map Val Val) -> Table

data Val = StringVal String | NumVal Double | BoolVal Bool | NilVal | VoidVal | FuncVal Unique Int LuaFunc | TableVal Unique Table | LabelVal StatList

constructorTag :: Val -> Int
constructorTag (StringVal _) = 0
constructorTag (NumVal _) = 1
constructorTag (BoolVal _) = 2
constructorTag NilVal = 3
constructorTag VoidVal = 4
constructorTag (FuncVal {}) = 5
constructorTag (TableVal {}) = 6
constructorTag (LabelVal _) = 7

instance Ord Val where
  compare x y =
    case compare (constructorTag x) (constructorTag y) of
      Prelude.EQ -> compareSame x y
      ord -> ord

compareSame :: Val -> Val -> Ordering
compareSame (StringVal x) (StringVal y) = compare x y
compareSame (NumVal x) (NumVal y) = compare x y
compareSame (BoolVal x) (BoolVal y) = compare x y
compareSame NilVal NilVal = Prelude.EQ
compareSame VoidVal VoidVal = Prelude.EQ
compareSame (FuncVal x _ _) (FuncVal y _ _) = compare (hashUnique x) (hashUnique y)
compareSame (TableVal x _) (TableVal y _) = compare (hashUnique x) (hashUnique y)
compareSame (LabelVal _) (LabelVal _) = Prelude.EQ
compareSame _ _ = error "compareSame: mismatched constructors"


instance Eq Val where
  (StringVal x) == (StringVal y) = x == y
  (NumVal x) == (NumVal y) = x == y
  (BoolVal x) == (BoolVal y) = x == y
  NilVal == NilVal = True
  VoidVal == VoidVal = True
  (FuncVal x _ _) == (FuncVal y _ _) = x == y
  (TableVal x _) == (TableVal y _) = x == y
  (LabelVal x) == (LabelVal y) = x == y
  _ == _ = False

instance Show Val where
  show (StringVal x) = x
  show (NumVal x) = if fromIntegral (round x) == x then show $ round x else show x
  show (BoolVal x) = map toLower $ show x
  show NilVal = "nil"
  show VoidVal = error "internal error: attempt to show void value"
  show (FuncVal x _ _) = "function: " ++ show (hashUnique x)
  show (TableVal x _) = "table: " ++ show (hashUnique x)
  

type LuaFunc = GlobalEnv -> Env -> [Val] -> IO(GlobalEnv, Env, [Val])



data GlobalEnv = GlobalEnv { vars :: Map String Val, collator :: Collator }

instance Show GlobalEnv where
  show x = "GlobalEnv {" ++ show (vars x) ++ "}"

instance Eq GlobalEnv where
  x == y = vars x == vars y

data Env = Env (Map String Val) [Val] Env | EnvEmpty
  deriving(Eq, Show)
