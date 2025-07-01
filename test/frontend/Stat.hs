module Stat where

import AST
import Helpers (testTemplate)
import Parser.Parser
import Test.HUnit
import Text.Parsec (parse)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)

assign1 :: Test
assign1 =
  testTemplate
    ("Parses simple assign statement " ++ show str)
    (Asgn (VarList [Var $ PrefixName "x" PrefixEmpty]) (ExprList [LiteralExpr (StringLit "hello")]))
    stat
    str
  where
    str = "x = \"hello\""

assign2 :: Test
assign2 =
  testTemplate
    ("Parses complex assign statement " ++ show str)
    ( Asgn
        ( VarList
            [ Var $ PrefixName "x" $ DotIndex "dotindex" PrefixEmpty,
              Var $ PrefixName "y" $ TableIndex (LiteralExpr (NumLit (NumInt 5))) PrefixEmpty,
              Var $ PrefixName "world" PrefixEmpty
            ]
        )
        ( ExprList
            [ BinExpr (LiteralExpr (NumLit (NumDouble 5.07))) CONCAT (LiteralExpr (StringLit "1")),
              PreExpr $ PrefixName "fn" (CallArgs (ArgList $ ExprList []) PrefixEmpty),
              LiteralExpr (StringLit "hello"),
              NIL
            ]
        )
    )
    stat
    str
  where
    str = "x.dotindex, y[5], world = 5.07 .. \"1\", fn(), \"hello\", --[===[multi\nline\ncomment]===] nil"

assignTests :: Test
assignTests = TestList [assign1, assign2]

label1 :: Test
label1 =
  testTemplate
    ("Parses label " ++ show str)
    (AST.Label "labelname")
    stat
    str
  where
    str = "::labelname::"

labelTests :: Test
labelTests = TestList [label1]

semic1 :: Test
semic1 =
  testTemplate
    "Parses semicolon (empty statement)"
    Semic
    stat
    "--inline comment\n  \t;"

semicTests :: Test
semicTests = TestList [semic1]

break1 :: Test
break1 =
  testTemplate
    "Parses break statement"
    Break
    stat
    "break"

breakTests :: Test
breakTests = TestList [break1]

goto1 :: Test
goto1 =
  testTemplate
    "Parses goto statement"
    (Goto "label_name")
    stat
    "goto label_name"

gotoTests :: Test
gotoTests = TestList [goto1]

do1 :: Test
do1 =
  testTemplate
    "Parses empty do block"
    (Do $ Block (StatList []) Nothing)
    stat
    "do end"

do2 :: Test
do2 =
  testTemplate
    "Parses empty do block with return"
    ( Do $
        Block
          (StatList [])
          ( Just $
              RetStat
                ( ExprList
                    [ LiteralExpr $ StringLit "val1",
                      PreExpr $ PrefixName "t" $ DotIndex "field" PrefixEmpty
                    ]
                )
          )
    )
    stat
    "do return \"val1\",t.field end"

doTests :: Test
doTests = TestList [do1, do2]

whileDo1 :: Test
whileDo1 = testTemplate "Parses empty while do" expected stat input
  where
    expected = WhileDo TRUE (Block (StatList []) Nothing)
    input = "while true do end"

whileDo2 :: Test
whileDo2 = testTemplate "Parses nested while do with return" expected stat input
  where
    expected = WhileDo TRUE (Block (StatList [WhileDo TRUE (Block (StatList [Break, Semic]) Nothing)]) (Just $ RetStat $ ExprList [NIL]))
    input = "while true do while true do break; end return nil end"

whileDoTests :: Test
whileDoTests = TestList [whileDo1, whileDo2]

repeatUntil1 :: Test
repeatUntil1 = testTemplate "Parses empty repeat until" expected stat input
  where
    expected = RepeatUntil (Block (StatList []) Nothing) FALSE
    input = "repeat until false"

repeatUntil2 :: Test
repeatUntil2 = testTemplate "Parses nested repeat until with return" expected stat input
  where
    expected = RepeatUntil (Block (StatList [RepeatUntil (Block (StatList [Break, Semic]) Nothing) FALSE]) (Just $ RetStat $ ExprList [LiteralExpr (StringLit "retval")])) TRUE
    input = "repeat repeat break; until false return \"retval\"; until true"

repeatUntilTests :: Test
repeatUntilTests = TestList [repeatUntil1, repeatUntil2]

funcCall1 :: Test
funcCall1 = testTemplate "Parses simle function call" expected stat "fn()"
  where
    expected = FuncCallStat $ FuncCall $ PrefixName "fn" (CallArgs (ArgList (ExprList [])) PrefixEmpty)

funcCall2 :: Test
funcCall2 = testTemplate "Parses function call with string args" expected stat "fn \"arg\""
  where
    expected = FuncCallStat $ FuncCall $ PrefixName "fn" (CallArgs (ArgString (LiteralExpr (StringLit "arg"))) PrefixEmpty)

funcCall3 :: Test
funcCall3 = testTemplate "Parses function call with string args" expected stat "fn 'arg'"
  where
    expected = FuncCallStat $ FuncCall $ PrefixName "fn" (CallArgs (ArgString (LiteralExpr (StringLit "arg"))) PrefixEmpty)

funcCall4 :: Test
funcCall4 = testTemplate "Parses function call with multi line string args" expected stat "fn [====[\narg\n]====]"
  where
    expected = FuncCallStat $ FuncCall $ PrefixName "fn" (CallArgs (ArgString (LiteralExpr (StringLit "\narg\n"))) PrefixEmpty)

funcCall5 :: Test
funcCall5 = testTemplate "Parses function call with multiple args" expected stat "fn('stringarg', true, false, nil, {})"
  where
    expected = FuncCallStat $ FuncCall $ PrefixName "fn" (CallArgs (ArgList (ExprList [LiteralExpr (StringLit "stringarg"), TRUE, FALSE, NIL, TableExpr (TableConstructor [])])) PrefixEmpty)

funcCall6 :: Test
funcCall6 = testTemplate "Parses method function call" expected stat "fn:m()"
  where
    expected = FuncCallStat $ FuncCall $ PrefixName "fn" (MethodArgs "m" (ArgList (ExprList [])) PrefixEmpty)

funcCall7 :: Test
funcCall7 = testTemplate "Parses function call with table constructor args" expected stat "fn {}"
  where
    expected = FuncCallStat $ FuncCall $ PrefixName "fn" (CallArgs (ArgTable (TableConstructor [])) PrefixEmpty)

funcCallTests :: Test
funcCallTests = TestList [funcCall1, funcCall2, funcCall3, funcCall4, funcCall5, funcCall6, funcCall7]

if1 :: Test
if1 = testTemplate "Parses simple if statement" expected stat input
  where
    expected = IfStat TRUE (Block (StatList []) Nothing) (ElseIfList []) Nothing
    input = "if true then end"

if2 :: Test
if2 = testTemplate "Parses if-elseif statement" expected stat input
  where
    expected = IfStat FALSE (Block (StatList []) (Just $ RetStat (ExprList [TRUE]))) (ElseIfList [ElseIf TRUE (Block (StatList []) (Just $ RetStat (ExprList [NIL])))]) Nothing
    input = "if false then return true elseif true then return nil end"

if3 :: Test
if3 = testTemplate "Parses if with multiple elseif statements" expected stat input
  where
    expected = IfStat FALSE (Block (StatList []) (Just $ RetStat (ExprList [TRUE]))) (ElseIfList [ElseIf TRUE (Block (StatList []) (Just $ RetStat (ExprList [NIL]))), ElseIf NIL (Block (StatList []) (Just $ RetStat (ExprList [FALSE])))]) Nothing
    input = "if false then return true elseif true then return nil elseif nil then return false end"

if4 :: Test
if4 = testTemplate "Parses simple if-else statement" expected stat input
  where
    expected = IfStat TRUE (Block (StatList []) (Just $ RetStat (ExprList [TRUE]))) (ElseIfList []) (Just $ Else (Block (StatList []) (Just $ RetStat (ExprList [NIL, FALSE]))))
    input = "if true then return true; else return nil, false end"

if5 :: Test
if5 = testTemplate "Parses if with multiple elseif statements and else" expected stat input
  where
    expected = IfStat FALSE (Block (StatList []) (Just $ RetStat (ExprList [TRUE]))) (ElseIfList [ElseIf TRUE (Block (StatList []) (Just $ RetStat (ExprList [NIL]))), ElseIf NIL (Block (StatList []) (Just $ RetStat (ExprList [FALSE])))]) (Just $ Else (Block (StatList []) (Just $ RetStat (ExprList [NIL]))))
    input = "if false then return true elseif true then return nil elseif nil then return false else return nil end"

ifTests :: Test
ifTests = TestList [if1, if2, if3, if4, if5]

for1 :: Test
for1 = testTemplate "Parses simple for-loop" expected stat input
  where
    expected = ForStat "i" (LiteralExpr (NumLit (NumInt 1))) (LiteralExpr (NumLit (NumInt 10))) Nothing (Block (StatList [FuncCallStat $ FuncCall $ PrefixName "fn" (CallArgs (ArgList $ ExprList []) PrefixEmpty)]) Nothing)
    input = "for i = 1,10 do fn() end"

for2 :: Test
for2 = testTemplate "Parses for-loop with three index vars" expected stat input
  where
    expected = ForStat "i" (LiteralExpr (NumLit (NumInt 1))) (LiteralExpr (NumLit (NumInt 10))) (Just (LiteralExpr (NumLit (NumDouble 0.5)))) (Block (StatList [FuncCallStat $ FuncCall $ PrefixName "fn" (CallArgs (ArgList $ ExprList []) PrefixEmpty)]) Nothing)
    input = "for i = 1,10,0.5 do fn() end"

forTests :: Test
forTests = TestList [for1, for2]

forIn1 :: Test
forIn1 = testTemplate "Parses simple for-in-loop" expected stat input
  where
    expected = ForIn (NameList ["a"]) (ExprList [LiteralExpr (NumLit (NumInt 1)), TRUE, NIL]) (Block (StatList [FuncCallStat $ FuncCall $ PrefixName "fn" (CallArgs (ArgList $ ExprList []) PrefixEmpty)]) Nothing)
    input = "for a in 1, true, nil do fn() end"

forIn2 :: Test
forIn2 = testTemplate "Parses for-in-loop with multiple index vars" expected stat input
  where
    expected = ForIn (NameList ["a", "b", "c", "d"]) (ExprList [LiteralExpr (NumLit (NumInt 1)), TRUE, NIL]) (Block (StatList [FuncCallStat $ FuncCall $ PrefixName "fn" (CallArgs (ArgList $ ExprList []) PrefixEmpty)]) Nothing)
    input = "for a, b,c,d in 1, true, nil do fn() end"

forInTests :: Test
forInTests = TestList [forIn1, forIn2]

funcDef1 :: Test
funcDef1 = testTemplate "Parses simple function definition statement" expected stat input
  where
    expected = FuncDefStat (FuncName "fn" [] Nothing) (FuncBody (ParamList (NameList []) Nothing) (Block (StatList []) Nothing))
    input = "function fn() end"

funcDef2 :: Test
funcDef2 = testTemplate "Parses function definition for table" expected stat input
  where
    expected = FuncDefStat (FuncName "t" ["fn"] Nothing) (FuncBody (ParamList (NameList []) Nothing) (Block (StatList []) Nothing))
    input = "function t.fn() end"

funcDef3 :: Test
funcDef3 = testTemplate "Parses complex function definition for table with method" expected stat input
  where
    expected = FuncDefStat (FuncName "t" ["field", "field2"] (Just "fn")) (FuncBody (ParamList (NameList ["a", "b", "c"]) (Just VarArg)) (Block (StatList []) (Just $ RetStat (ExprList [TRUE]))))
    input = "function t.field.field2:fn(a,b,c, ...) return true; end"

funcDefTests :: Test
funcDefTests = TestList [funcDef1, funcDef2, funcDef3]

localFunc1 :: Test
localFunc1 = testTemplate "Parses local function definition statement" expected stat input
  where
    expected = LocalFuncStat "foo" (FuncBody (ParamList (NameList ["x", "y"]) (Just VarArg)) (Block (StatList []) (Just $ RetStat (ExprList [TRIPLE_DOT]))))
    input = "local function foo(x, y, ...) return ... end"

localFuncTests :: Test
localFuncTests = TestList [localFunc1]

localAsgn1 :: Test
localAsgn1 = testTemplate "Parses simple local assignment with no expression" expected stat input
  where
    expected = LocalAsgn (AttrNameList [("x", Nothing)]) Nothing
    input = "local x"

localAsgn2 :: Test
localAsgn2 = testTemplate "Parses simple local assignment" expected stat input
  where
    expected = LocalAsgn (AttrNameList [("x", Nothing)]) (Just (ExprList [TRUE]))
    input = "local x = true"

localAsgn3 :: Test
localAsgn3 = testTemplate "Parses simple local assignment with attribute" expected stat input
  where
    expected = LocalAsgn (AttrNameList [("x", Just $ Attrib "const")]) (Just (ExprList [TRUE]))
    input = "local x <const> = true"

localAsgn4 :: Test
localAsgn4 = testTemplate "Parses local assignment with two variables, no attributes" expected stat input
  where
    expected = LocalAsgn (AttrNameList [("x", Nothing), ("y", Nothing)]) (Just (ExprList [TRUE]))
    input = "local x, y = true"

localAsgn5 :: Test
localAsgn5 = testTemplate "Parses local assignment with two variables, one attribute" expected stat input
  where
    expected = LocalAsgn (AttrNameList [("x", Just $ Attrib "const"), ("y", Nothing)]) (Just (ExprList [TRUE, TRIPLE_DOT]))
    input = "local x <const>, y = true, ..."

localAsgn6 :: Test
localAsgn6 = testTemplate "Parses local assignment with two variables, two attributes" expected stat input
  where
    expected = LocalAsgn (AttrNameList [("x", Just $ Attrib "const"), ("y", Just $ Attrib "close")]) (Just (ExprList [FALSE, NIL]))
    input = "local x <const>, y<close> = false,nil"

localAsgnTests :: Test
localAsgnTests = TestList [localAsgn1, localAsgn2, localAsgn3, localAsgn4, localAsgn5, localAsgn6]

statTests :: Test
statTests = TestList [assignTests, labelTests, semicTests, breakTests, gotoTests, doTests, whileDoTests, repeatUntilTests, funcCallTests, ifTests, forTests, forInTests, funcDefTests, localFuncTests, localAsgnTests]
