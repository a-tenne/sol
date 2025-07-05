module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import Expr (exprTests)
import Runtime (runtimeTests)
import Stat (statTests)
import Interpreter (programTests)

tests = TestList [exprTests, runtimeTests, statTests, programTests]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess
