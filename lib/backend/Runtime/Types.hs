{-# LANGUAGE GADTs #-}
module Runtime.Types where

import Data.Map(Map)
import Data.Char (toLower)
import Data.Text.ICU (Collator)
import AST
import GHC.IO (unsafePerformIO)
import GHC.StableName (makeStableName, hashStableName)

data Table where
  Table :: (Map Val Val) -> Table

data Val = StringVal String | NumVal Double | BoolVal Bool | NilVal | VoidVal | NatFuncVal NativeFunc | TableVal Table | LabelVal StatList

instance Eq Val where
  (StringVal x) == (StringVal y) = x == y
  (NumVal x) == (NumVal y) = x == y
  (NatFuncVal x) == (NatFuncVal y) = 
    unsafePerformIO $ do
      name1 <- makeStableName x
      name2 <- makeStableName y
      return (name1 == name2)
  (TableVal x) == (TableVal y) =
    unsafePerformIO $ do
      name1 <- makeStableName x
      name2 <- makeStableName y
      return (name1 == name2)
  (BoolVal x) == (BoolVal y) = x == y
  NilVal == NilVal = True
  VoidVal == VoidVal = True
  _ == _ = False

type NativeFunc = GlobalEnv -> Env -> [Val] -> IO(GlobalEnv, [Val])

instance Show Val where
  show (StringVal x) = x
  show (NumVal x) = if fromIntegral (round x) == x then show $ round x else show x
  show (BoolVal x) = map toLower $ show x
  show NilVal = "nil"
  show VoidVal = error "attempt to show void value"
  show (NatFuncVal x) =
    unsafePerformIO $ do
      n <- makeStableName x
      return $ "function: " ++ show (hashStableName n)
  show (TableVal x) =
    unsafePerformIO $ do
      n <- makeStableName x
      return $ "table: " ++ show (hashStableName n)

data GlobalEnv = GlobalEnv { vars :: Map String Val, collator :: Collator }

instance Show GlobalEnv where
  show x = "GlobalEnv {" ++ show (vars x) ++ "}"

instance Eq GlobalEnv where
  x == y = vars x == vars y

data Env = Env (Map String Val) Env | EnvEmpty
  deriving(Eq, Show)

