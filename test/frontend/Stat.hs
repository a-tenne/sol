module Stat where

import AST
import Helpers (parseWrapper, testTemplate)
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
    expected = WhileDo TRUE (Block (StatList [WhileDo TRUE (Block (StatList [Break, Semic]) Nothing)] ) (Just $ RetStat  $ ExprList [NIL]))
    input = "while true do while true do break; end return nil end"

whileDoTests :: Test
whileDoTests = TestList [whileDo1, whileDo2]

repeatUntilTests :: Test
repeatUntilTests = TestList []

statTests :: Test
statTests = TestList [assignTests, labelTests, semicTests, breakTests, gotoTests, doTests, whileDoTests]
