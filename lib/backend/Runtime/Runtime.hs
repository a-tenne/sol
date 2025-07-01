module Runtime.Runtime where
import Runtime.Types
import Runtime.Expr
import qualified Data.Text.ICU as ICU
import Data.Map (empty)

initialEnv = Env {vars = empty, collator = ICU.collator ICU.Current}
