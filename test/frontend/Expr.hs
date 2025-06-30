module Expr where

import AST
import Helpers (parseWrapper, testTemplate)
import Parser.Parser
import Test.HUnit
import Text.Parsec.String (Parser)

num1 :: Test
num1 = testTemplate "Parses 42 as number" (LiteralExpr (NumLit (NumInt 42))) ex1 "42"

num2 :: Test
num2 = testTemplate "Parses 0x52 as number" (LiteralExpr (NumLit (NumInt 0x52))) ex1 "0x52"

num3 :: Test
num3 = testTemplate "Parses 123.321 as number" (LiteralExpr (NumLit (NumDouble 123.321))) ex1 "123.321"

numTests :: Test
numTests = TestList [num1, num2, num3]

table1 :: Test
table1 = testTemplate "Parses empty table constructor" (TableExpr (TableConstructor [])) ex1 "{}"

table2 :: Test
table2 = testTemplate "Parses table constructor" (TableExpr (TableConstructor [])) ex1 "{}"
  where
    field1 = SingleExField (LiteralExpr (StringLit "field_expr"))
    field2 = ExField (UnaryExpr U_NOT (BinExpr (LiteralExpr (NumLit (NumInt 10))) CONCAT (LiteralExpr (StringLit "aaa")))) (LiteralExpr (NumLit (NumDouble 5.5)))
    field3 = NamedField "field_name" (PreExpr $ PrefixName "fn" $ CallArgs (ArgList $ ExprList []) PrefixEmpty)
    expect = TableExpr (TableConstructor [field1, field2, field3])
    input = "{\"field_expr\", [10 .. \"aaa\"] = 5.5, field_name = fn()}"

tableTests :: Test
tableTests = TestList [table1, table2]

string1 :: Test
string1 =
  testTemplate
    "Parses escaped single line string"
    (LiteralExpr (StringLit end))
    ex1
    start
  where
    start = "\"hello\\r\\n\\t\\\\\""
    end = "hello\r\n\t\\"

string2 :: Test
string2 =
  testTemplate
    "Parses multi line string"
    (LiteralExpr (StringLit end))
    ex1
    start
  where
    start = "[==[hello\n\\thaskell\\r\\nworld]==]"
    end = "hello\n\\thaskell\\r\\nworld"

stringTests :: Test
stringTests = TestList [string1, string2]

primitive1 :: Test
primitive1 = testTemplate "Parses nil" NIL ex1 "nil"

primitive2 :: Test
primitive2 = testTemplate "Parses true" TRUE ex1 "true"

primitive3 :: Test
primitive3 = testTemplate "Parses false" FALSE ex1 "false"

primitive4 :: Test
primitive4 = testTemplate "Parses ... (varargs)" TRIPLE_DOT ex1 "..."

primitiveTests :: Test
primitiveTests = TestList [primitive1, primitive2, primitive3, primitive4]

function1 :: Test
function1 = testTemplate ("Parses parameter list (no varargs)" ++ input) expected paramList input
  where
    expected = ParamList (NameList ["x", "y", "z", "a", "b", "c"]) Nothing
    input = "(x,y,z,a,b,c)"

function2 :: Test
function2 = testTemplate ("Parses parameter list (with varargs)" ++ input) expected paramList input
  where
    expected = ParamList (NameList ["x", "y", "z"]) $ Just VarArg
    input = "(x,y,z,...)"

function3 :: Test
function3 = testTemplate "Parses empty function expression" expected ex1 "function() end"
  where
    expected = FunctionDef (FuncBody (ParamList (NameList []) Nothing) (Block (StatList []) Nothing))

function4 :: Test
function4 = testTemplate "Parses complex function expression" expected ex1 input
  where
    expected = FunctionDef (FuncBody (ParamList (NameList ["a","b"]) (Just VarArg)) (Block (StatList []) (Just $ RetStat $ ExprList [LiteralExpr (StringLit "retval")])))
    input = "function(a,b, ...) return \"retval\" end"

functionTests :: Test
functionTests = TestList [function1, function2, function3, function4]

complex :: Test
complex =
  testTemplate
    ("Parses complex expression " ++ exprStr)
    expected
    ex1
    exprStr
  where
    exprStr = "#(~4 // -0x5.3p-2 ^ (0x5 + 80.623 * \"0x12\"))"
    expected =
      UnaryExpr
        U_LEN
        ( PreExpr $
            PrefixSub
              ( BinExpr
                  (UnaryExpr U_B_NOT (LiteralExpr $ NumLit (NumInt 4)))
                  INT_DIV
                  ( UnaryExpr
                      U_MINUS
                      ( BinExpr
                          (LiteralExpr $ NumLit (NumDouble 1.296875))
                          EXP
                          ( PreExpr $
                              PrefixSub
                                ( BinExpr
                                    (LiteralExpr $ NumLit (NumInt 5))
                                    PLUS
                                    ( BinExpr
                                        (LiteralExpr $ NumLit (NumDouble 80.623))
                                        MULT
                                        (LiteralExpr $ StringLit "0x12")
                                    )
                                )
                                PrefixEmpty
                          )
                      )
                  )
              )
              PrefixEmpty
        )

exprTests :: Test
exprTests = TestList [numTests, tableTests, stringTests, primitiveTests, functionTests, complex]
