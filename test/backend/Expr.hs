module Expr where

import AST (Literal (StringLit))
import Control.Exception (SomeException, evaluate, try)
import Data.Map qualified as M
import Runtime.Interpreter
import Runtime.Runtime
import Runtime.Types
import Test.HUnit
import Text.Parsec (parse)
import Parser.Parser
import Text.Parsec.Token (GenTokenParser(reserved))


l = EnvEmpty

coerce1 :: Test
coerce1 = TestCase $ assertEqual "Coerces the string '10' to a number" (NumVal 10.0) (coerce "10")

coerce2 :: Test
coerce2 = TestCase $ assertEqual "Coerces the string '0xFf' to a number" (NumVal 255.0) (coerce "0xFf")

coerce3 :: Test
coerce3 = TestCase $ assertEqual "Coerces the string '0x5.3p-2' to a number" (NumVal 1.296875) (coerce "0x5.3p-2")

coerce4 :: Test
coerce4 = TestCase $ assertEqual "Coerces the string '80.623' to a number" (NumVal 80.623) (coerce "80.623")

coerceTests :: Test
coerceTests = TestList [coerce1, coerce2, coerce3, coerce4]

arithFns :: [(Double -> Double -> Double, String)]
arithFns = [((+), "addition"), ((-), "subtraction"), ((*), "multiplication"), ((/), "division"), ((**), "exponantiation")]

arithTemplate :: Val -> Val -> Test
arithTemplate (NumVal a) (NumVal b) = TestList $ map (\(x, y) -> TestCase $ do
  g <- initialEnv
  (_,_, res) <- arith x g l  (NumVal a) (NumVal b)
  assertEqual ("Evaluates " ++ y ++ " between " ++ show a ++ " and " ++ show b) (NumVal $ x a b) res ) arithFns

arithTemplate (StringVal a) (StringVal b) = TestList $ map (\(x, y) -> TestCase $ do
  g <- initialEnv
  (_,_,res) <- arith x g l (NumVal a2) (NumVal b2)
  assertEqual ("Evaluates " ++ y ++ " between " ++ show a ++ " and " ++ show b) (NumVal $ x a2 b2) res) arithFns
  where
    (NumVal a2) = coerce a
    (NumVal b2) = coerce b
arithTemplate (NumVal a) (StringVal b) = TestList $ map (\(x, y) -> TestCase $ do
  g <- initialEnv
  (_,_,res) <- arith x g l (NumVal a) (NumVal b2)
  assertEqual ("Evaluates " ++ y ++ " between " ++ show a ++ " and " ++ show b) (NumVal $ x a b2) res) arithFns
  where
    (NumVal b2) = coerce b
arithTemplate (StringVal a) (NumVal b) = arithTemplate (NumVal b) (StringVal a)
arithTemplate a b = TestList $ map (\(x, y) -> crashTemplate (arith x) y a b) arithFns

crashTemplate :: BinFn -> String -> Val -> Val -> Test
crashTemplate fn name a b = TestCase $ do
  g <- initialEnv
  result <- try (fn g l a b) :: IO (Either SomeException (GlobalEnv, Env, Val))
  case result of
    Left _ -> return ()
    Right _ ->
      assertFailure $
        "Expected operation " ++ name ++ " between " ++ show a ++ " and " ++ show b ++ " to throw, but it didn't"

arith1 :: Test
arith1 = arithTemplate (NumVal 13.0) (NumVal 5.0)

arith2 = arithTemplate (StringVal "0x10") (StringVal "20.3")

arith3 = arithTemplate (StringVal "10") (NumVal 20.6)

arith4 = arithTemplate (NumVal 0xF) (StringVal "0x1.5p-2")

arith5 = arithTemplate (BoolVal True) (NumVal 10.0)

arith6 = arithTemplate NilVal (NumVal 2000.75)

arith7 :: Test
arith7 = TestCase $ do
  g <- initialEnv
  (_, _, NumVal result) <- arith (/) g l (NumVal 0) (NumVal 0)
  assertEqual "Evaluates division between 0 and 0" True (isNaN result)

arithTests :: Test
arithTests = TestList [arith1, arith2, arith3, arith4, arith5, arith6, arith7]

intDiv1 :: Test
intDiv1 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- intDiv g l a b
  assertEqual ("Evaluates integer division between " ++ show a ++ " and " ++ show b) expected res
  where
    x = 205.23
    y = 3.6
    expected = NumVal $ fromIntegral $ floor (x / y)
    a = NumVal x
    b = NumVal y

intDiv2 :: Test
intDiv2 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- intDiv g l a b
  assertEqual ("Evaluates integer division between " ++ show a ++ " and " ++ show b) expected res
  where
    x = 0x60.3p-2
    y = 20.3
    expected =NumVal $ fromIntegral $ floor (x / y)
    a = NumVal x
    b = StringVal (show y)

intDiv3 :: Test
intDiv3 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- intDiv g l a b
  assertEqual ("Evaluates integer division between string " ++ show a ++ " and " ++ show b) expected res
  where
    x = 0xFF
    y = 0x10
    expected =NumVal $ fromIntegral $ floor (x / y)
    a = StringVal (show x)
    b = NumVal y

intDiv4 :: Test
intDiv4 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- intDiv g l a b
  assertEqual ("Evaluates integer division between string " ++ show a ++ " and string " ++ show b) expected res
  where
    x = 30000
    y = 5400
    expected =NumVal $ fromIntegral $ floor (x / y)
    a = StringVal (show x)
    b = StringVal (show y)

intDiv5 :: Test
intDiv5 = crashTemplate intDiv "integer division" NilVal (NumVal 10.3)

intDiv6 :: Test
intDiv6 = crashTemplate intDiv "integer division" (BoolVal True) (NumVal 10.3)

intDiv7 :: Test
intDiv7 = crashTemplate intDiv "integer division" (BoolVal True) (NumVal 10.3)

intDivTests :: Test
intDivTests = TestList [intDiv1, intDiv2, intDiv3, intDiv4, intDiv5, intDiv6, intDiv7]

concat1 :: Test
concat1 = TestCase $ do
  g <- initialEnv
  (_,_,res ) <- valConcat g l a b
  assertEqual ("String concatenation between " ++ show a ++ " and " ++ show b) expected res
  where
    x = 10.5
    y = 555
    expected =StringVal "10.5555"
    a = NumVal x
    b = NumVal y
concat2 :: Test
concat2 = TestCase $ do
  g <- initialEnv
  (_,_,res ) <- valConcat g l a b
  assertEqual ("String concatenation between " ++ show a ++ " and " ++ show b) expected res
  where
    x = "str1"
    y = "str2"
    expected =StringVal $ x ++ y
    a = StringVal x
    b = StringVal y

concat3 :: Test
concat3 = crashTemplate valConcat "concatenation" (BoolVal True) (BoolVal False)

concat4 :: Test
concat4 = crashTemplate valConcat "concatenation" NilVal NilVal

concatTests :: Test
concatTests = TestList [concat1, concat2, concat3, concat4]

andTest1 :: Test
andTest1 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valAnd g l a b
  assertEqual ("Evaluates and between " ++ show a ++ ", " ++ show b) expected res
  where
    a = BoolVal True
    b = StringVal "expected"
    expected = b
andTest2 :: Test
andTest2 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valAnd g l a b
  assertEqual ("Evaluates and between " ++ show a ++ ", " ++ show b) expected res
  where
    a = BoolVal False
    b = StringVal "unexpected"
    expected = a
andTest3 :: Test
andTest3 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valAnd g l a b
  assertEqual ("Evaluates and between " ++ show a ++ ", " ++ show b) expected res
  where
    a = NilVal
    b = StringVal "unexpected"
    expected = a

andTests :: Test
andTests = TestList [andTest1, andTest2, andTest3]

orTest1 :: Test
orTest1 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valOr g l a b
  assertEqual ("Evaluates or between " ++ show a ++ ", " ++ show b) expected res
  where
    a = BoolVal True
    b = StringVal "unexpected"
    expected = a
orTest2 :: Test
orTest2 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valOr g l a b
  assertEqual ("Evaluates or between " ++ show a ++ ", " ++ show b) expected res
  where
    a = BoolVal False
    b = StringVal "expected"
    expected = b
orTest3 :: Test
orTest3 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valOr g l a b
  assertEqual ("Evaluates or between " ++ show a ++ ", " ++ show b) expected res
  where
    a = NilVal
    b = StringVal "expected"
    expected = b

orTests :: Test
orTests = TestList [orTest1, orTest2, orTest3]

eqTest1 :: Test
eqTest1 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valEq g l a b
  assertEqual ("Evaluates equality between " ++ show a ++ " and " ++ show b) expected res
  where
    expected =BoolVal True
    a = BoolVal True
    b = BoolVal True

eqTest2 :: Test
eqTest2 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valEq g l a b
  assertEqual ("Evaluates equality between " ++ show a ++ " and " ++ show b) expected res
  where
    expected =BoolVal False
    a = BoolVal True
    b = StringVal "true"


eqTest3 :: Test
eqTest3 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valEq g l a b
  assertEqual ("Evaluates equality between " ++ show a ++ " and " ++ show b) expected res
  where
    expected =BoolVal True
    a = StringVal "str"
    b = StringVal "str"
eqTest4 :: Test
eqTest4 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valEq g l a b
  assertEqual ("Evaluates equality between " ++ show a ++ " and " ++ show b) expected res
  where
    expected =BoolVal False
    a = NumVal 1
    b = NumVal 2

eqTests :: Test
eqTests = TestList [eqTest1, eqTest2, eqTest3, eqTest4]
neqTest1 :: Test
neqTest1 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valNe g l a b
  assertEqual ("Evaluates inequality between " ++ show a ++ " and " ++ show b) expected res
  where
    expected =BoolVal False
    a = BoolVal True
    b = BoolVal True

neqTest2 :: Test
neqTest2 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valNe g l a b
  assertEqual ("Evaluates inequality between " ++ show a ++ " and " ++ show b) expected res
  where
    expected =BoolVal True
    a = BoolVal True
    b = StringVal "true"


neqTest3 :: Test
neqTest3 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valNe g l a b
  assertEqual ("Evaluates inequality between " ++ show a ++ " and " ++ show b) expected res
  where
    expected =BoolVal False
    a = StringVal "str"
    b = StringVal "str"
neqTest4 :: Test
neqTest4 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valNe g l a b
  assertEqual ("Evaluates inequality between " ++ show a ++ " and " ++ show b) expected res
  where
    expected =BoolVal True
    a = NumVal 1
    b = NumVal 2

neqTests :: Test
neqTests = TestList [neqTest1, neqTest2, neqTest3, neqTest4]

gt1 :: Test
gt1 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valGt g l a b
  assertEqual ("Evaluates " ++ show a ++ " " ++ operation ++ " " ++ show b) expected res
  where
    expected =BoolVal True
    operation = ">"
    a = StringVal "abc"
    b = StringVal "abb"
gt2 :: Test
gt2 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valGt g l a b
  assertEqual ("Evaluates " ++ show a ++ " " ++ operation ++ " " ++ show b) expected res
  where
    expected =BoolVal False
    operation = ">"
    a = NumVal 5
    b = NumVal 6

gt3 :: Test
gt3 = crashTemplate valGt "greater" a b
  where
    a = StringVal "1"
    b = NumVal 0
gt4 :: Test
gt4 = crashTemplate valGt "greater" a b
  where
    a = NilVal
    b = NilVal

gtTests :: Test
gtTests = TestList [gt1, gt2, gt3, gt4]
lt1 :: Test
lt1 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valLt g l a b
  assertEqual ("Evaluates " ++ show a ++ " " ++ operation ++ " " ++ show b) expected res
  where
    expected =BoolVal False
    operation = "<"
    a = StringVal "abc"
    b = StringVal "abb"
lt2 :: Test
lt2 = TestCase $ do
  g <- initialEnv
  (_,_,res) <- valLt g l a b
  assertEqual ("Evaluates " ++ show a ++ " " ++ operation ++ " " ++ show b) expected res
  where
    expected =BoolVal True
    operation = "<"
    a = NumVal 5
    b = NumVal 6

lt3 :: Test
lt3 = crashTemplate valGt "less" a b
  where
    a = StringVal "1"
    b = NumVal 0
lt4 :: Test
lt4 = crashTemplate valGt  "less" a b
  where
    a = NilVal
    b = NilVal
ltTests :: Test
ltTests = TestList [lt1, lt2, lt3, lt4]

geTests :: Test
geTests = TestList []

leTests :: Test
leTests = TestList []

modTests :: Test
modTests = TestList []

bAndTests :: Test
bAndTests = TestList []

bOrTests :: Test
bOrTests = TestList []

bXorTests :: Test
bXorTests = TestList []

lshiftTests :: Test
lshiftTests = TestList []

rshiftTests :: Test
rshiftTests = TestList []

interpretE1 :: Test
interpretE1 = TestCase $ do
  g <- initialEnv
  (_,_, res) <- interpretE g l tree
  assertEqual "Evaluates simple addition between numbers" [NumVal 6] res
  where
    (Right tree) = parse ex1 "" "1+2+3"

interpretE2 :: Test
interpretE2 = TestCase $ do
  g <- initialEnv
  (_,_, res) <- interpretE g l tree
  assertEqual "Evaluates simple addition and multiplication between numbers" [NumVal 7] res
  where
    (Right tree) = parse ex1 "" "1+2*3"

interpretE3 :: Test
interpretE3 = TestCase $ do
  g <- initialEnv
  (_,_, res) <- interpretE g l tree
  assertEqual ("Evaluates complex expression " ++ show ex) [NumVal (-1)] res
  where
    ex = "200000000 // -0x5.3p-2 ^ (0X5 + 80.623 * '0x12')"
    (Right tree) = parse ex1 "" ex

interpretETests :: Test
interpretETests = TestList [interpretE1, interpretE2, interpretE3]

exprTests :: Test
exprTests = TestList [coerceTests, arithTests, intDivTests, concatTests, andTests, orTests, eqTests, neqTests, gtTests, ltTests, geTests, leTests, modTests, bAndTests, bOrTests, bXorTests, lshiftTests, rshiftTests, interpretETests]
