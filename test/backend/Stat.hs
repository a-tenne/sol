module Stat where
import Test.HUnit
import Runtime.Runtime 
import Runtime.Types
import System.IO.Silently (capture)
import Data.List (intercalate)
import Runtime.Stat
import Text.Parsec (parse)
import Parser.Parser (program)

l = EnvEmpty

funcCall1 :: Test
funcCall1 = TestCase $ do
  g <- initialEnv
  let (Right ast) = parse program "" "print('hello, world')"
  (output, _) <- capture $ interpret ast
  assertEqual ("Print outputs " ++ show expected) expected output
    where
      input = [StringVal "hello, world"]
      expected = intercalate "\t" (map show input) ++ "\n"


funcCallTests :: Test
funcCallTests = TestList [funcCall1]

statTests :: Test
statTests = TestList [funcCallTests]
