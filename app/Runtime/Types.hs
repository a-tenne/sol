module Runtime.Types where

import Data.Map(Map)
import Data.Char (toLower)
import Data.Text.ICU (Collator)

data Val = StringVal String | NumVal Double | BoolVal Bool | NilVal
  deriving (Eq)

instance Show Val where
  show (StringVal x) = x
  show (NumVal x) = show x
  show (BoolVal x) = map toLower $ show x
  show NilVal = "nil"

data Env = Env { vars :: Map String Val, collator :: Collator }
