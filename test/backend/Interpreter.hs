module Interpreter where

import Parser.Parser (program)
import Runtime.Interpreter
import System.IO.Silently (capture)
import Test.HUnit
import Text.Parsec (parse)
import Data.List (intercalate)

template :: String -> String -> String -> Test
template label input expected = TestCase $ do
  let (Right ast) = parse program "" input
  (output, _) <- capture $ interpret ast
  assertEqual label expected output

table1 :: Test
table1 =
  template
    "Tables should be modified in-place"
    "function x(a) a.b = 10 end local t = {} x(t) print(t.b)"
    "10\n"

table2 :: Test
table2 =
  template
    "Nested table reassignment and access"
    "t = {a={b={{}}}} t['a'].b[1].c = 10 print(t['a'].b[1].c)"
    "10\n"

tableTests :: Test
tableTests = TestList [table1, table2]

while1 :: Test
while1 =
  template
    "Basic while loop"
    "i = 0 while i < 5 do print(i) i = i + 1 end"
    "0\n1\n2\n3\n4\n"

while2 :: Test
while2 =
  template
    "While loop never enters"
    "i = 0 while i > 0 do print(i) i = i + 1 end print 'success'"
    "success\n"

while3 :: Test
while3 =
  template
    "While loop breaks on break statement"
    "i = 0 while i < 5 do print(i) i = i + 1 break end"
    "0\n"

whileTests :: Test
whileTests = TestList [while1, while2, while3]

forLoop1 :: Test
forLoop1 =
  template
    "Basic for loop"
    "for i = 1,10 do print(i) end"
    (concatMap (\x -> show x ++ "\n") [1..10])

forLoop2 :: Test
forLoop2 =
  template
    "For loop breaks on break statement"
    "for i = 1,10 do print(i) break end"
    "1\n"

forLoop3 :: Test
forLoop3 =
  template
    "For loop with different increment number"
    "for i = 1,10,2 do print(i) end"
    "1\n3\n5\n7\n9\n"

forLoop4 :: Test
forLoop4 =
  template
    "For loop never enters"
    "for i = 1,-10 do print(i) end"
    ""

forLoop5 :: Test
forLoop5 =
  template
    "For loop reverse"
    "for i = 0,-5,-1 do print(i) end"
    "0\n-1\n-2\n-3\n-4\n-5\n"

forLoopTests :: Test
forLoopTests = TestList [forLoop1, forLoop2, forLoop3, forLoop4, forLoop5]

repeat1 :: Test
repeat1 =
  template
    "Basic repeat until"
    "local i = 5 repeat print(i) i = i - 1 until i == 0"
    "5\n4\n3\n2\n1\n"

repeat2 :: Test
repeat2 =
  template
    "Repeat until breaks immediately"
    "local i = 1 repeat print(i) i = i - 1 until true"
    "1\n"

repeat3 :: Test
repeat3 =
  template
    "Repeat until breaks on break statement"
    "local i = 1 repeat print(i) i = i - 1 break until false"
    "1\n"

repeatUntilTests :: Test
repeatUntilTests = TestList [repeat1, repeat2, repeat3]

goto1 :: Test
goto1 = 
  template
    "Goto loop"
    "i=0 ::start:: print(i) i = i + 1 if i <= 10 then goto start end"
    (concatMap (\x -> show x ++ "\n") [0..10])

goto2 :: Test
goto2 =
  template
    "Goto skips to end"
    "goto label for i = 1,10 do print(i) end ::label:: print 'success'"
    "success\n"

gotoTests :: Test
gotoTests = TestList [goto1, goto2]

assign1 :: Test
assign1 =
  template
    "Multiple assignment, nil default"
    "a,b,c,d,e = 1,0xff,true,'hello' print(a,b,c,d,e)"
    "1\t255\ttrue\thello\tnil\n"

assign2 :: Test
assign2 =
  template
    "Multiple assignment, redundant expressions get ignored"
    "a,b = 'hello', false, 0xFF"
    ""

assignTests :: Test
assignTests = TestList [assign1, assign2]

programTests :: Test
programTests = TestList [tableTests, whileTests, assignTests, forLoopTests, repeatUntilTests, gotoTests]
