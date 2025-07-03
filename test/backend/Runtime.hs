module Runtime where
import Test.HUnit
import Runtime.Runtime
import System.IO.Silently(capture)
import Runtime.Types
import Data.List (intercalate)

l = EnvEmpty

print1 :: Test
print1 = TestCase $ do
  g <- initialEnv
  (output, _) <- capture $ luaPrint g l input
  assertEqual ("Print outputs " ++ show expected) expected output
    where
      input = formatVals [StringVal "hello, world"]
      expected = intercalate "\t" (map show input) ++ "\n"
print2 :: Test
print2 = TestCase $ do
  g <- initialEnv
  (output, _) <- capture $ luaPrint g l input
  assertEqual ("Print outputs " ++ show expected ++ " with void as first parameter") expected output
    where
      input = formatVals [VoidVal, NumVal 10]
      expected = "nil\t10\n"

print3 :: Test
print3 = TestCase $ do
  g <- initialEnv
  (output, _) <- capture $ luaPrint g l input
  assertEqual "Print outputs nothing" expected output
    where
      input = formatVals [VoidVal]
      expected = "\n"
print4 :: Test
print4 = TestCase $ do
  g <- initialEnv
  (output, _) <- capture $ luaPrint g l input
  assertEqual ("Print outputs " ++ show expected) expected output
    where
      input = formatVals [NumVal 1, NumVal 2, NumVal 3, VoidVal]
      expected = "1\t2\t3\n"
print5 :: Test
print5 = TestCase $ do
  g <- initialEnv
  (output, _) <- capture $ luaPrint g l input
  assertEqual ("Print outputs " ++ show expected) expected output
    where
      input = formatVals [StringVal "hello", NumVal 1, BoolVal True, BoolVal False, NilVal]
      expected = intercalate "\t" (map show input) ++ "\n"


printTests :: Test
printTests = TestList [print1,print2,print3,print4,print5]

runtimeTests :: Test
runtimeTests = TestList [printTests]
